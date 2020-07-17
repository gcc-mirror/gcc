/* Offload image generation tool for AMD GCN.

   Copyright (C) 2014-2020 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Munges GCN assembly into a C source file defining the GCN code as a
   string.

   This is not a complete assembler.  We presume the source is well
   formed from the compiler and can die horribly if it is not.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "obstack.h"
#include "diagnostic.h"
#include "intl.h"
#include <libgen.h>
#include "collect-utils.h"
#include "gomp-constants.h"
#include "simple-object.h"
#include "elf.h"

/* These probably won't (all) be in elf.h for a while.  */
#undef  EM_AMDGPU
#define EM_AMDGPU		0xe0;

#undef  ELFOSABI_AMDGPU_HSA
#define ELFOSABI_AMDGPU_HSA	 64
#undef  ELFABIVERSION_AMDGPU_HSA
#define ELFABIVERSION_AMDGPU_HSA 1

#undef  EF_AMDGPU_MACH_AMDGCN_GFX803
#define EF_AMDGPU_MACH_AMDGCN_GFX803 0x2a
#undef  EF_AMDGPU_MACH_AMDGCN_GFX900
#define EF_AMDGPU_MACH_AMDGCN_GFX900 0x2c
#undef  EF_AMDGPU_MACH_AMDGCN_GFX906
#define EF_AMDGPU_MACH_AMDGCN_GFX906 0x2f

#ifndef R_AMDGPU_NONE
#define R_AMDGPU_NONE		0
#define R_AMDGPU_ABS32_LO	1	/* (S + A) & 0xFFFFFFFF  */
#define R_AMDGPU_ABS32_HI	2	/* (S + A) >> 32  */
#define R_AMDGPU_ABS64		3	/* S + A  */
#define R_AMDGPU_REL32		4	/* S + A - P  */
#define R_AMDGPU_REL64		5	/* S + A - P  */
#define R_AMDGPU_ABS32		6	/* S + A  */
#define R_AMDGPU_GOTPCREL	7	/* G + GOT + A - P  */
#define R_AMDGPU_GOTPCREL32_LO	8	/* (G + GOT + A - P) & 0xFFFFFFFF  */
#define R_AMDGPU_GOTPCREL32_HI	9	/* (G + GOT + A - P) >> 32  */
#define R_AMDGPU_REL32_LO	10	/* (S + A - P) & 0xFFFFFFFF  */
#define R_AMDGPU_REL32_HI	11	/* (S + A - P) >> 32  */
#define R_AMDGPU_RELATIVE64	13	/* B + A  */
#endif

const char tool_name[] = "gcn mkoffload";

static const char *gcn_dumpbase;
static struct obstack files_to_cleanup;

enum offload_abi offload_abi = OFFLOAD_ABI_UNSET;
uint32_t elf_arch = EF_AMDGPU_MACH_AMDGCN_GFX803;  // Default GPU architecture.

/* Delete tempfiles.  */

void
tool_cleanup (bool from_signal ATTRIBUTE_UNUSED)
{
  obstack_ptr_grow (&files_to_cleanup, NULL);
  const char **files = XOBFINISH (&files_to_cleanup, const char **);
  for (int i = 0; files[i]; i++)
    maybe_unlink (files[i]);
}

static void
mkoffload_cleanup (void)
{
  tool_cleanup (false);
}

/* Unlink FILE unless requested otherwise.  */

void
maybe_unlink (const char *file)
{
  if (!save_temps)
    {
      if (unlink_if_ordinary (file) && errno != ENOENT)
	fatal_error (input_location, "deleting file %s: %m", file);
    }
  else if (verbose)
    fprintf (stderr, "[Leaving %s]\n", file);
}

/* Add or change the value of an environment variable, outputting the
   change to standard error if in verbose mode.  */

static void
xputenv (const char *string)
{
  if (verbose)
    fprintf (stderr, "%s\n", string);
  putenv (CONST_CAST (char *, string));
}

/* Read the whole input file.  It will be NUL terminated (but
   remember, there could be a NUL in the file itself.  */

static const char *
read_file (FILE *stream, size_t *plen)
{
  size_t alloc = 16384;
  size_t base = 0;
  char *buffer;

  if (!fseek (stream, 0, SEEK_END))
    {
      /* Get the file size.  */
      long s = ftell (stream);
      if (s >= 0)
	alloc = s + 100;
      fseek (stream, 0, SEEK_SET);
    }
  buffer = XNEWVEC (char, alloc);

  for (;;)
    {
      size_t n = fread (buffer + base, 1, alloc - base - 1, stream);

      if (!n)
	break;
      base += n;
      if (base + 1 == alloc)
	{
	  alloc *= 2;
	  buffer = XRESIZEVEC (char, buffer, alloc);
	}
    }
  buffer[base] = 0;
  *plen = base;
  return buffer;
}

/* Parse STR, saving found tokens into PVALUES and return their number.
   Tokens are assumed to be delimited by ':'.  */

static unsigned
parse_env_var (const char *str, char ***pvalues)
{
  const char *curval, *nextval;
  char **values;
  unsigned num = 1, i;

  curval = strchr (str, ':');
  while (curval)
    {
      num++;
      curval = strchr (curval + 1, ':');
    }

  values = (char **) xmalloc (num * sizeof (char *));
  curval = str;
  nextval = strchr (curval, ':');
  if (nextval == NULL)
    nextval = strchr (curval, '\0');

  for (i = 0; i < num; i++)
    {
      int l = nextval - curval;
      values[i] = (char *) xmalloc (l + 1);
      memcpy (values[i], curval, l);
      values[i][l] = 0;
      curval = nextval + 1;
      nextval = strchr (curval, ':');
      if (nextval == NULL)
	nextval = strchr (curval, '\0');
    }
  *pvalues = values;
  return num;
}

/* Auxiliary function that frees elements of PTR and PTR itself.
   N is number of elements to be freed.  If PTR is NULL, nothing is freed.
   If an element is NULL, subsequent elements are not freed.  */

static void
free_array_of_ptrs (void **ptr, unsigned n)
{
  unsigned i;
  if (!ptr)
    return;
  for (i = 0; i < n; i++)
    {
      if (!ptr[i])
	break;
      free (ptr[i]);
    }
  free (ptr);
  return;
}

/* Check whether NAME can be accessed in MODE.  This is like access,
   except that it never considers directories to be executable.  */

static int
access_check (const char *name, int mode)
{
  if (mode == X_OK)
    {
      struct stat st;

      if (stat (name, &st) < 0 || S_ISDIR (st.st_mode))
	return -1;
    }

  return access (name, mode);
}

/* Copy the early-debug-info from the incoming LTO object to a new object
   that will be linked into the output HSACO file.  The host relocations
   must be translated into GCN relocations, and any global undefined symbols
   must be weakened (so as not to have the debug info try to pull in host
   junk).

   Returns true if the file was created, false otherwise.  */

static bool
copy_early_debug_info (const char *infile, const char *outfile)
{
  const char *errmsg;
  int err;

  /* The simple_object code can handle extracting the debug sections.
     This code is based on that in lto-wrapper.c.  */
  int infd = open (infile, O_RDONLY | O_BINARY);
  if (infd == -1)
    return false;
  simple_object_read *inobj = simple_object_start_read (infd, 0,
							"__GNU_LTO",
							&errmsg, &err);
  if (!inobj)
    return false;

  off_t off, len;
  if (simple_object_find_section (inobj, ".gnu.debuglto_.debug_info",
				  &off, &len, &errmsg, &err) != 1)
    {
      simple_object_release_read (inobj);
      close (infd);
      return false;
    }

  errmsg = simple_object_copy_lto_debug_sections (inobj, outfile, &err, true);
  if (errmsg)
    {
      unlink_if_ordinary (outfile);
      return false;
    }

  simple_object_release_read (inobj);
  close (infd);

  /* Open the file we just created for some adjustments.
     The simple_object code can't do this, so we do it manually.  */
  FILE *outfd = fopen (outfile, "r+b");
  if (!outfd)
    return false;

  Elf64_Ehdr ehdr;
  if (fread (&ehdr, sizeof (ehdr), 1, outfd) != 1)
    {
      fclose (outfd);
      return true;
    }

  /* We only support host relocations of x86_64, for now.  */
  gcc_assert (ehdr.e_machine == EM_X86_64);

  /* Patch the correct elf architecture flag into the file.  */
  ehdr.e_ident[7] = ELFOSABI_AMDGPU_HSA;
  ehdr.e_ident[8] = ELFABIVERSION_AMDGPU_HSA;
  ehdr.e_type = ET_REL;
  ehdr.e_machine = EM_AMDGPU;
  ehdr.e_flags = elf_arch;

  /* Load the section headers so we can walk them later.  */
  Elf64_Shdr *sections = (Elf64_Shdr *)xmalloc (sizeof (Elf64_Shdr)
						* ehdr.e_shnum);
  if (fseek (outfd, ehdr.e_shoff, SEEK_SET) == -1
      || fread (sections, sizeof (Elf64_Shdr), ehdr.e_shnum,
		outfd) != ehdr.e_shnum)
    {
      free (sections);
      fclose (outfd);
      return true;
    }

  /* Convert the host relocations to target relocations.  */
  for (int i = 0; i < ehdr.e_shnum; i++)
    {
      if (sections[i].sh_type != SHT_RELA)
	continue;

      char *data = (char *)xmalloc (sections[i].sh_size);
      if (fseek (outfd, sections[i].sh_offset, SEEK_SET) == -1
	  || fread (data, sections[i].sh_size, 1, outfd) != 1)
	{
	  free (data);
	  continue;
	}

      for (size_t offset = 0;
	   offset < sections[i].sh_size;
	   offset += sections[i].sh_entsize)
	{
	  Elf64_Rela *reloc = (Elf64_Rela *) (data + offset);

	  /* Map the host relocations to GCN relocations.
	     Only relocations that can appear in DWARF need be handled.  */
	  switch (ELF64_R_TYPE (reloc->r_info))
	    {
	    case R_X86_64_32:
	    case R_X86_64_32S:
	      reloc->r_info = R_AMDGPU_ABS32;
	      break;
	    case R_X86_64_PC32:
	      reloc->r_info = R_AMDGPU_REL32;
	      break;
	    case R_X86_64_PC64:
	      reloc->r_info = R_AMDGPU_REL64;
	      break;
	    case R_X86_64_64:
	      reloc->r_info = R_AMDGPU_ABS64;
	      break;
	    case R_X86_64_RELATIVE:
	      reloc->r_info = R_AMDGPU_RELATIVE64;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}

      /* Write back our relocation changes.  */
      if (fseek (outfd, sections[i].sh_offset, SEEK_SET) != -1)
	fwrite (data, sections[i].sh_size, 1, outfd);

      free (data);
    }

  /* Weaken any global undefined symbols that would pull in unwanted
     objects.  */
  for (int i = 0; i < ehdr.e_shnum; i++)
    {
      if (sections[i].sh_type != SHT_SYMTAB)
	continue;

      char *data = (char *)xmalloc (sections[i].sh_size);
      if (fseek (outfd, sections[i].sh_offset, SEEK_SET) == -1
	  || fread (data, sections[i].sh_size, 1, outfd) != 1)
	{
	  free (data);
	  continue;
	}

      for (size_t offset = 0;
	   offset < sections[i].sh_size;
	   offset += sections[i].sh_entsize)
	{
	  Elf64_Sym *sym = (Elf64_Sym *) (data + offset);
	  int type = ELF64_ST_TYPE (sym->st_info);
	  int bind = ELF64_ST_BIND (sym->st_info);

	  if (bind == STB_GLOBAL && sym->st_shndx == 0)
	    sym->st_info = ELF64_ST_INFO (STB_WEAK, type);
	}

      /* Write back our symbol changes.  */
      if (fseek (outfd, sections[i].sh_offset, SEEK_SET) != -1)
	fwrite (data, sections[i].sh_size, 1, outfd);

      free (data);
    }
  free (sections);

  /* Write back our header changes.  */
  rewind (outfd);
  fwrite (&ehdr, sizeof (ehdr), 1, outfd);

  fclose (outfd);
  return true;
}

/* Parse an input assembler file, extract the offload tables etc.,
   and output (1) the assembler code, minus the tables (which can contain
   problematic relocations), and (2) a C file with the offload tables
   encoded as structured data.  */

static void
process_asm (FILE *in, FILE *out, FILE *cfile)
{
  int fn_count = 0, var_count = 0, dims_count = 0, regcount_count = 0;
  struct obstack fns_os, vars_os, varsizes_os, dims_os, regcounts_os;
  obstack_init (&fns_os);
  obstack_init (&vars_os);
  obstack_init (&varsizes_os);
  obstack_init (&dims_os);
  obstack_init (&regcounts_os);

  struct oaccdims
  {
    int d[3];
    char *name;
  } dim;

  struct regcount
  {
    int sgpr_count;
    int vgpr_count;
    char *kernel_name;
  } regcount;

  /* Always add _init_array and _fini_array as kernels.  */
  obstack_ptr_grow (&fns_os, xstrdup ("_init_array"));
  obstack_ptr_grow (&fns_os, xstrdup ("_fini_array"));
  fn_count += 2;

  char buf[1000];
  enum { IN_CODE, IN_AMD_KERNEL_CODE_T, IN_VARS, IN_FUNCS } state = IN_CODE;
  while (fgets (buf, sizeof (buf), in))
    {
      switch (state)
	{
	case IN_CODE:
	  {
	    if (sscanf (buf, " ;; OPENACC-DIMS: %d, %d, %d : %ms\n",
			&dim.d[0], &dim.d[1], &dim.d[2], &dim.name) == 4)
	      {
		obstack_grow (&dims_os, &dim, sizeof (dim));
		dims_count++;
	      }
	    else if (sscanf (buf, " .amdgpu_hsa_kernel %ms\n",
			     &regcount.kernel_name) == 1)
	      break;

	    break;
	  }
	case IN_AMD_KERNEL_CODE_T:
	  {
	    gcc_assert (regcount.kernel_name);
	    if (sscanf (buf, " wavefront_sgpr_count = %d\n",
			&regcount.sgpr_count) == 1)
	      break;
	    else if (sscanf (buf, " workitem_vgpr_count = %d\n",
			     &regcount.vgpr_count) == 1)
	      break;

	    break;
	  }
	case IN_VARS:
	  {
	    char *varname;
	    unsigned varsize;
	    if (sscanf (buf, " .8byte %ms\n", &varname))
	      {
		obstack_ptr_grow (&vars_os, varname);
		fgets (buf, sizeof (buf), in);
		if (!sscanf (buf, " .8byte %u\n", &varsize))
		  abort ();
		obstack_int_grow (&varsizes_os, varsize);
		var_count++;

		/* The HSA Runtime cannot locate the symbol if it is not
		   exported from the kernel.  */
		fprintf (out, "\t.global %s\n", varname);
	      }
	    break;
	  }
	case IN_FUNCS:
	  {
	    char *funcname;
	    if (sscanf (buf, "\t.8byte\t%ms\n", &funcname))
	      {
		obstack_ptr_grow (&fns_os, funcname);
		fn_count++;
		continue;
	      }
	    break;
	  }
	}

      char dummy;
      if (sscanf (buf, " .section .gnu.offload_vars%c", &dummy) > 0)
	state = IN_VARS;
      else if (sscanf (buf, " .section .gnu.offload_funcs%c", &dummy) > 0)
	state = IN_FUNCS;
      else if (sscanf (buf, " .amd_kernel_code_%c", &dummy) > 0)
	{
	  state = IN_AMD_KERNEL_CODE_T;
	  regcount.sgpr_count = regcount.vgpr_count = -1;
	}
      else if (sscanf (buf, " .section %c", &dummy) > 0
	       || sscanf (buf, " .text%c", &dummy) > 0
	       || sscanf (buf, " .bss%c", &dummy) > 0
	       || sscanf (buf, " .data%c", &dummy) > 0
	       || sscanf (buf, " .ident %c", &dummy) > 0)
	state = IN_CODE;
      else if (sscanf (buf, " .end_amd_kernel_code_%c", &dummy) > 0)
	{
	  state = IN_CODE;
	  gcc_assert (regcount.kernel_name != NULL
		      && regcount.sgpr_count >= 0
		      && regcount.vgpr_count >= 0);
	  obstack_grow (&regcounts_os, &regcount, sizeof (regcount));
	  regcount_count++;
	  regcount.kernel_name = NULL;
	  regcount.sgpr_count = regcount.vgpr_count = -1;
	}

      if (state == IN_CODE || state == IN_AMD_KERNEL_CODE_T)
	fputs (buf, out);
    }

  char **fns = XOBFINISH (&fns_os, char **);
  struct oaccdims *dims = XOBFINISH (&dims_os, struct oaccdims *);
  struct regcount *regcounts = XOBFINISH (&regcounts_os, struct regcount *);

  fprintf (cfile, "#include <stdlib.h>\n");
  fprintf (cfile, "#include <stdbool.h>\n\n");

  char **vars = XOBFINISH (&vars_os, char **);
  unsigned *varsizes = XOBFINISH (&varsizes_os, unsigned *);
  fprintf (cfile,
	   "static const struct global_var_info {\n"
	   "  const char *name;\n"
	   "  void *address;\n"
	   "} vars[] = {\n");
  int i;
  for (i = 0; i < var_count; ++i)
    {
      const char *sep = i < var_count - 1 ? "," : " ";
      fprintf (cfile, "  { \"%s\", NULL }%s /* size: %u */\n", vars[i], sep,
	       varsizes[i]);
    }
  fprintf (cfile, "};\n\n");

  obstack_free (&vars_os, NULL);
  obstack_free (&varsizes_os, NULL);

  /* Dump out function idents.  */
  fprintf (cfile, "static const struct hsa_kernel_description {\n"
	   "  const char *name;\n"
	   "  int oacc_dims[3];\n"
	   "  int sgpr_count;\n"
	   "  int vgpr_count;\n"
	   "} gcn_kernels[] = {\n  ");
  dim.d[0] = dim.d[1] = dim.d[2] = 0;
  const char *comma;
  for (comma = "", i = 0; i < fn_count; comma = ",\n  ", i++)
    {
      /* Find if we recorded dimensions for this function.  */
      int *d = dim.d;		/* Previously zeroed.  */
      int sgpr_count = 0;
      int vgpr_count = 0;
      for (int j = 0; j < dims_count; j++)
	if (strcmp (fns[i], dims[j].name) == 0)
	  {
	    d = dims[j].d;
	    break;
	  }
      for (int j = 0; j < regcount_count; j++)
	if (strcmp (fns[i], regcounts[j].kernel_name) == 0)
	  {
	    sgpr_count = regcounts[j].sgpr_count;
	    vgpr_count = regcounts[j].vgpr_count;
	    break;
	  }

      fprintf (cfile, "%s{\"%s\", {%d, %d, %d}, %d, %d}", comma,
	       fns[i], d[0], d[1], d[2], sgpr_count, vgpr_count);

      free (fns[i]);
    }
  fprintf (cfile, "\n};\n\n");

  obstack_free (&fns_os, NULL);
  for (i = 0; i < dims_count; i++)
    free (dims[i].name);
  for (i = 0; i < regcount_count; i++)
    free (regcounts[i].kernel_name);
  obstack_free (&dims_os, NULL);
  obstack_free (&regcounts_os, NULL);
}

/* Embed an object file into a C source file.  */

static void
process_obj (FILE *in, FILE *cfile)
{
  size_t len = 0;
  const char *input = read_file (in, &len);

  /* Dump out an array containing the binary.
     FIXME: do this with objcopy.  */
  fprintf (cfile, "static unsigned char gcn_code[] = {");
  for (size_t i = 0; i < len; i += 17)
    {
      fprintf (cfile, "\n\t");
      for (size_t j = i; j < i + 17 && j < len; j++)
	fprintf (cfile, "%3u,", (unsigned char) input[j]);
    }
  fprintf (cfile, "\n};\n\n");

  fprintf (cfile,
	   "static const struct gcn_image {\n"
	   "  size_t size;\n"
	   "  void *image;\n"
	   "} gcn_image = {\n"
	   "  %zu,\n"
	   "  gcn_code\n"
	   "};\n\n",
	   len);

  fprintf (cfile,
	   "static const struct gcn_image_desc {\n"
	   "  const struct gcn_image *gcn_image;\n"
	   "  unsigned kernel_count;\n"
	   "  const struct hsa_kernel_description *kernel_infos;\n"
	   "  unsigned global_variable_count;\n"
	   "  const struct global_var_info *global_variables;\n"
	   "} target_data = {\n"
	   "  &gcn_image,\n"
	   "  sizeof (gcn_kernels) / sizeof (gcn_kernels[0]),\n"
	   "  gcn_kernels,\n"
	   "  sizeof (vars) / sizeof (vars[0]),\n"
	   "  vars\n"
	   "};\n\n");

  fprintf (cfile,
	   "#ifdef __cplusplus\n"
	   "extern \"C\" {\n"
	   "#endif\n"
	   "extern void GOMP_offload_register_ver"
	   " (unsigned, const void *, int, const void *);\n"
	   "extern void GOMP_offload_unregister_ver"
	   " (unsigned, const void *, int, const void *);\n"
	   "#ifdef __cplusplus\n"
	   "}\n"
	   "#endif\n\n");

  fprintf (cfile, "extern const void *const __OFFLOAD_TABLE__[];\n\n");

  fprintf (cfile, "static __attribute__((constructor)) void init (void)\n"
	   "{\n"
	   "  GOMP_offload_register_ver (%#x, __OFFLOAD_TABLE__,"
	   " %d/*GCN*/, &target_data);\n"
	   "};\n",
	   GOMP_VERSION_PACK (GOMP_VERSION, GOMP_VERSION_GCN),
	   GOMP_DEVICE_GCN);

  fprintf (cfile, "static __attribute__((destructor)) void fini (void)\n"
	   "{\n"
	   "  GOMP_offload_unregister_ver (%#x, __OFFLOAD_TABLE__,"
	   " %d/*GCN*/, &target_data);\n"
	   "};\n",
	   GOMP_VERSION_PACK (GOMP_VERSION, GOMP_VERSION_GCN),
	   GOMP_DEVICE_GCN);
}

/* Compile a C file using the host compiler.  */

static void
compile_native (const char *infile, const char *outfile, const char *compiler,
		bool fPIC, bool fpic)
{
  const char *collect_gcc_options = getenv ("COLLECT_GCC_OPTIONS");
  if (!collect_gcc_options)
    fatal_error (input_location,
		 "environment variable COLLECT_GCC_OPTIONS must be set");

  struct obstack argv_obstack;
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, compiler);
  if (fPIC)
    obstack_ptr_grow (&argv_obstack, "-fPIC");
  if (fpic)
    obstack_ptr_grow (&argv_obstack, "-fpic");
  if (save_temps)
    obstack_ptr_grow (&argv_obstack, "-save-temps");
  if (verbose)
    obstack_ptr_grow (&argv_obstack, "-v");
  obstack_ptr_grow (&argv_obstack, "-dumpdir");
  obstack_ptr_grow (&argv_obstack, "");
  obstack_ptr_grow (&argv_obstack, "-dumpbase");
  obstack_ptr_grow (&argv_obstack, gcn_dumpbase);
  obstack_ptr_grow (&argv_obstack, "-dumpbase-ext");
  obstack_ptr_grow (&argv_obstack, ".c");
  switch (offload_abi)
    {
    case OFFLOAD_ABI_LP64:
      obstack_ptr_grow (&argv_obstack, "-m64");
      break;
    case OFFLOAD_ABI_ILP32:
      obstack_ptr_grow (&argv_obstack, "-m32");
      break;
    default:
      gcc_unreachable ();
    }
  obstack_ptr_grow (&argv_obstack, infile);
  obstack_ptr_grow (&argv_obstack, "-c");
  obstack_ptr_grow (&argv_obstack, "-o");
  obstack_ptr_grow (&argv_obstack, outfile);
  obstack_ptr_grow (&argv_obstack, NULL);

  const char **new_argv = XOBFINISH (&argv_obstack, const char **);
  fork_execute (new_argv[0], CONST_CAST (char **, new_argv), true);
  obstack_free (&argv_obstack, NULL);
}

int
main (int argc, char **argv)
{
  FILE *in = stdin;
  FILE *out = stdout;
  FILE *cfile = stdout;
  const char *outname = 0;

  const char *gcn_s1_name;
  const char *gcn_s2_name;
  const char *gcn_o_name;
  const char *gcn_cfile_name;

  progname = "mkoffload";
  diagnostic_initialize (global_dc, 0);

  obstack_init (&files_to_cleanup);
  if (atexit (mkoffload_cleanup) != 0)
    fatal_error (input_location, "atexit failed");

  char *collect_gcc = getenv ("COLLECT_GCC");
  if (collect_gcc == NULL)
    fatal_error (input_location, "COLLECT_GCC must be set.");
  const char *gcc_path = dirname (ASTRDUP (collect_gcc));
  const char *gcc_exec = basename (ASTRDUP (collect_gcc));

  size_t len = (strlen (gcc_path) + 1 + strlen (GCC_INSTALL_NAME) + 1);
  char *driver = XALLOCAVEC (char, len);

  if (strcmp (gcc_exec, collect_gcc) == 0)
    /* collect_gcc has no path, so it was found in PATH.  Make sure we also
       find accel-gcc in PATH.  */
    gcc_path = NULL;

  int driver_used = 0;
  if (gcc_path != NULL)
    driver_used = sprintf (driver, "%s/", gcc_path);
  sprintf (driver + driver_used, "%s", GCC_INSTALL_NAME);

  bool found = false;
  if (gcc_path == NULL)
    found = true;
  else if (access_check (driver, X_OK) == 0)
    found = true;
  else
    {
      /* Don't use alloca pointer with XRESIZEVEC.  */
      driver = NULL;
      /* Look in all COMPILER_PATHs for GCC_INSTALL_NAME.  */
      char **paths = NULL;
      unsigned n_paths;
      n_paths = parse_env_var (getenv ("COMPILER_PATH"), &paths);
      for (unsigned i = 0; i < n_paths; i++)
	{
	  len = strlen (paths[i]) + 1 + strlen (GCC_INSTALL_NAME) + 1;
	  driver = XRESIZEVEC (char, driver, len);
	  sprintf (driver, "%s/%s", paths[i], GCC_INSTALL_NAME);
	  if (access_check (driver, X_OK) == 0)
	    {
	      found = true;
	      break;
	    }
	}
      free_array_of_ptrs ((void **) paths, n_paths);
    }

  if (!found)
    fatal_error (input_location,
		 "offload compiler %s not found", GCC_INSTALL_NAME);

  /* We may be called with all the arguments stored in some file and
     passed with @file.  Expand them into argv before processing.  */
  expandargv (&argc, &argv);

  /* Scan the argument vector.  */
  bool fopenmp = false;
  bool fopenacc = false;
  bool fPIC = false;
  bool fpic = false;
  for (int i = 1; i < argc; i++)
    {
#define STR "-foffload-abi="
      if (strncmp (argv[i], STR, strlen (STR)) == 0)
	{
	  if (strcmp (argv[i] + strlen (STR), "lp64") == 0)
	    offload_abi = OFFLOAD_ABI_LP64;
	  else if (strcmp (argv[i] + strlen (STR), "ilp32") == 0)
	    offload_abi = OFFLOAD_ABI_ILP32;
	  else
	    fatal_error (input_location,
			 "unrecognizable argument of option " STR);
	}
#undef STR
      else if (strcmp (argv[i], "-fopenmp") == 0)
	fopenmp = true;
      else if (strcmp (argv[i], "-fopenacc") == 0)
	fopenacc = true;
      else if (strcmp (argv[i], "-fPIC") == 0)
	fPIC = true;
      else if (strcmp (argv[i], "-fpic") == 0)
	fpic = true;
      else if (strcmp (argv[i], "-save-temps") == 0)
	save_temps = true;
      else if (strcmp (argv[i], "-v") == 0)
	verbose = true;
      else if (strcmp (argv[i], "-dumpbase") == 0
	       && i + 1 < argc)
	dumppfx = argv[++i];
      else if (strcmp (argv[i], "-march=fiji") == 0)
	elf_arch = EF_AMDGPU_MACH_AMDGCN_GFX803;
      else if (strcmp (argv[i], "-march=gfx900") == 0)
	elf_arch = EF_AMDGPU_MACH_AMDGCN_GFX900;
      else if (strcmp (argv[i], "-march=gfx906") == 0)
	elf_arch = EF_AMDGPU_MACH_AMDGCN_GFX906;
    }

  if (!(fopenacc ^ fopenmp))
    fatal_error (input_location, "either -fopenacc or -fopenmp must be set");

  const char *abi;
  switch (offload_abi)
    {
    case OFFLOAD_ABI_LP64:
      abi = "-m64";
      break;
    case OFFLOAD_ABI_ILP32:
      abi = "-m32";
      break;
    default:
      gcc_unreachable ();
    }

  /* Build arguments for compiler pass.  */
  struct obstack cc_argv_obstack;
  obstack_init (&cc_argv_obstack);
  obstack_ptr_grow (&cc_argv_obstack, driver);
  obstack_ptr_grow (&cc_argv_obstack, "-S");

  if (save_temps)
    obstack_ptr_grow (&cc_argv_obstack, "-save-temps");
  if (verbose)
    obstack_ptr_grow (&cc_argv_obstack, "-v");
  obstack_ptr_grow (&cc_argv_obstack, abi);
  obstack_ptr_grow (&cc_argv_obstack, "-xlto");
  if (fopenmp)
    obstack_ptr_grow (&cc_argv_obstack, "-mgomp");

  for (int ix = 1; ix != argc; ix++)
    {
      if (!strcmp (argv[ix], "-o") && ix + 1 != argc)
	outname = argv[++ix];
      else
	obstack_ptr_grow (&cc_argv_obstack, argv[ix]);
    }

  if (!dumppfx)
    dumppfx = outname;

  const char *mko_dumpbase = concat (dumppfx, ".mkoffload", NULL);
  const char *hsaco_dumpbase = concat (dumppfx, ".mkoffload.hsaco", NULL);
  gcn_dumpbase = concat (dumppfx, ".c", NULL);

  if (save_temps)
    {
      gcn_s1_name = concat (mko_dumpbase, ".1.s", NULL);
      gcn_s2_name = concat (mko_dumpbase, ".2.s", NULL);
      gcn_o_name = hsaco_dumpbase;
      gcn_cfile_name = gcn_dumpbase;
    }
  else
    {
      gcn_s1_name = make_temp_file (".mkoffload.1.s");
      gcn_s2_name = make_temp_file (".mkoffload.2.s");
      gcn_o_name = make_temp_file (".mkoffload.hsaco");
      gcn_cfile_name = make_temp_file (".c");
    }
  obstack_ptr_grow (&files_to_cleanup, gcn_s1_name);
  obstack_ptr_grow (&files_to_cleanup, gcn_s2_name);
  obstack_ptr_grow (&files_to_cleanup, gcn_o_name);
  obstack_ptr_grow (&files_to_cleanup, gcn_cfile_name);

  obstack_ptr_grow (&cc_argv_obstack, "-dumpdir");
  obstack_ptr_grow (&cc_argv_obstack, "");
  obstack_ptr_grow (&cc_argv_obstack, "-dumpbase");
  obstack_ptr_grow (&cc_argv_obstack, mko_dumpbase);
  obstack_ptr_grow (&cc_argv_obstack, "-dumpbase-ext");
  obstack_ptr_grow (&cc_argv_obstack, "");

  obstack_ptr_grow (&cc_argv_obstack, "-o");
  obstack_ptr_grow (&cc_argv_obstack, gcn_s1_name);
  obstack_ptr_grow (&cc_argv_obstack, NULL);
  const char **cc_argv = XOBFINISH (&cc_argv_obstack, const char **);

  /* Build arguments for assemble/link pass.  */
  struct obstack ld_argv_obstack;
  obstack_init (&ld_argv_obstack);
  obstack_ptr_grow (&ld_argv_obstack, driver);

  /* Extract early-debug information from the input objects.
     This loop finds all the inputs that end ".o" and aren't the output.  */
  int dbgcount = 0;
  for (int ix = 1; ix != argc; ix++)
    {
      if (!strcmp (argv[ix], "-o") && ix + 1 != argc)
	++ix;
      else
	{
	  if (strcmp (argv[ix] + strlen(argv[ix]) - 2, ".o") == 0)
	    {
	      char *dbgobj;
	      if (save_temps)
		{
		  char buf[10];
		  sprintf (buf, "%d", dbgcount++);
		  dbgobj = concat (dumppfx, ".mkoffload.dbg", buf, ".o", NULL);
		}
	      else
		dbgobj = make_temp_file (".mkoffload.dbg.o");

	      /* If the copy fails then just ignore it.  */
	      if (copy_early_debug_info (argv[ix], dbgobj))
		{
		  obstack_ptr_grow (&ld_argv_obstack, dbgobj);
		  obstack_ptr_grow (&files_to_cleanup, dbgobj);
		}
	      else
		free (dbgobj);
	    }
	}
    }
  obstack_ptr_grow (&ld_argv_obstack, gcn_s2_name);
  obstack_ptr_grow (&ld_argv_obstack, "-lgomp");

  for (int i = 1; i < argc; i++)
    if (strncmp (argv[i], "-l", 2) == 0
	|| strncmp (argv[i], "-Wl", 3) == 0
	|| strncmp (argv[i], "-march", 6) == 0)
      obstack_ptr_grow (&ld_argv_obstack, argv[i]);

  obstack_ptr_grow (&cc_argv_obstack, "-dumpdir");
  obstack_ptr_grow (&cc_argv_obstack, "");
  obstack_ptr_grow (&cc_argv_obstack, "-dumpbase");
  obstack_ptr_grow (&cc_argv_obstack, hsaco_dumpbase);
  obstack_ptr_grow (&cc_argv_obstack, "-dumpbase-ext");
  obstack_ptr_grow (&cc_argv_obstack, "");

  obstack_ptr_grow (&ld_argv_obstack, "-o");
  obstack_ptr_grow (&ld_argv_obstack, gcn_o_name);
  obstack_ptr_grow (&ld_argv_obstack, NULL);
  const char **ld_argv = XOBFINISH (&ld_argv_obstack, const char **);

  /* Clean up unhelpful environment variables.  */
  char *execpath = getenv ("GCC_EXEC_PREFIX");
  char *cpath = getenv ("COMPILER_PATH");
  char *lpath = getenv ("LIBRARY_PATH");
  unsetenv ("GCC_EXEC_PREFIX");
  unsetenv ("COMPILER_PATH");
  unsetenv ("LIBRARY_PATH");

  /* Run the compiler pass.  */
  fork_execute (cc_argv[0], CONST_CAST (char **, cc_argv), true);
  obstack_free (&cc_argv_obstack, NULL);

  in = fopen (gcn_s1_name, "r");
  if (!in)
    fatal_error (input_location, "cannot open intermediate gcn asm file");

  out = fopen (gcn_s2_name, "w");
  if (!out)
    fatal_error (input_location, "cannot open '%s'", gcn_s2_name);

  cfile = fopen (gcn_cfile_name, "w");
  if (!cfile)
    fatal_error (input_location, "cannot open '%s'", gcn_cfile_name);

  process_asm (in, out, cfile);

  fclose (in);
  fclose (out);

  /* Run the assemble/link pass.  */
  fork_execute (ld_argv[0], CONST_CAST (char **, ld_argv), true);
  obstack_free (&ld_argv_obstack, NULL);

  in = fopen (gcn_o_name, "r");
  if (!in)
    fatal_error (input_location, "cannot open intermediate gcn obj file");

  process_obj (in, cfile);

  fclose (in);
  fclose (cfile);

  xputenv (concat ("GCC_EXEC_PREFIX=", execpath, NULL));
  xputenv (concat ("COMPILER_PATH=", cpath, NULL));
  xputenv (concat ("LIBRARY_PATH=", lpath, NULL));

  compile_native (gcn_cfile_name, outname, collect_gcc, fPIC, fpic);

  return 0;
}
