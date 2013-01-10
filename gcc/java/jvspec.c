/* Specific flags and argument handling of the front-end of the 
   GNU compiler for the Java(TM) language.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "gcc.h"
#include "jcf.h"
#include "opts.h"

/* Name of spec file.  */
#define SPEC_FILE "libgcj.spec"

/* This bit is set if we saw a `-xfoo' language specification.  */
#define LANGSPEC	(1<<1)
/* True if this arg is a .java input file name. */
#define JAVA_FILE_ARG	(1<<3)
/* True if this arg is a .class input file name. */
#define CLASS_FILE_ARG	(1<<4)
/* True if this arg is a .zip or .jar input file name. */
#define ZIP_FILE_ARG	(1<<5)
/* True if this arg is @FILE - where FILE contains a list of filenames. */
#define INDIRECT_FILE_ARG (1<<6)
/* True if this arg is a resource file.  */
#define RESOURCE_FILE_ARG (1<<7)

static char *find_spec_file (const char *);
static int verify_class_name (const char *);

static const char *main_class_name = NULL;
int lang_specific_extra_outfiles = 0;

/* True if we should add -shared-libgcc to the command-line.  */
int shared_libgcc = 1;

static const char jvgenmain_spec[] =
  "jvgenmain %{findirect-dispatch} %{D*} %b %m.i |\n\
   cc1 %m.i %1 \
		   %{!Q:-quiet} -dumpbase %b.c %{d*} %{m*}\
		   %{g*} %{O*} \
		   %{v:-version} %{pg:-p} %{p}\
		   %<fbounds-check %<fno-bounds-check\
		   %<fassume-compiled* %<fno-assume-compiled*\
		   %<fcompile-resource* %<fassert %<fno-assert \
		   %<femit-class-file %<femit-class-files %<fencoding*\
		   %<fuse-boehm-gc %<fhash-synchronization %<fjni\
		   %<findirect-dispatch\
		   %<fno-store-check %<foutput-class-dir\
		   %<fclasspath* %<fbootclasspath*\
		   %<fextdirs*\
		   %<fuse-divide-subroutine %<fno-use-divide-subroutine\
		   %<fuse-atomic-builtins %<fno-use-atomic-builtins\
		   %<fcheck-references %<fno-check-references\
		   %<ffilelist-file %<fsaw-java-file %<fsource* %<ftarget*\
		   %{f*} -fdollars-in-identifiers\
		   %{aux-info*}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}\
   %(invoke_as)";

/* Return full path name of spec file if it is in DIR, or NULL if
   not.  */
static char *
find_spec_file (const char *dir)
{
  char *spec;
  struct stat sb;

  spec = XNEWVEC (char, strlen (dir) + sizeof (SPEC_FILE) + 4);
  strcpy (spec, dir);
  strcat (spec, "/");
  strcat (spec, SPEC_FILE);
  if (! stat (spec, &sb))
    return spec;
  free (spec);
  return NULL;
}

#define JAVA_START_CHAR_P(c) (c < 128 && (ISIDST (c) || c == '$'))
#define JAVA_PART_CHAR_P(c) (c < 128					      \
			     && (ISIDNUM (c)				      \
				 || c == '$'				      \
				 || (c >= 0x00 && c <= 0x08)		      \
				 || (c >= 0x0e && c <= 0x1b)		      \
				 || c == 0x7f))

/* Verify that NAME is a valid Java class name that might contain
   `main'.  Return 0 on failure.  */
static int
verify_class_name (const char *name)
{
  /* FIXME: what encoding do we use for command-line arguments?  For
     now we assume plain ASCII, which of course is wrong.  */
  while (*name)
    {
      int ch = *name++;
      if (ch < 0 || ! JAVA_START_CHAR_P (ch))
	return 0;
      while (*name)
	{
	  ch = *name++;
	  if (ch < 0)
	    return 0;
	  /* We found a break between class names.  Next character
	     must be an identifier start again.  */
	  if (ch == '.')
	    break;
	  if (! JAVA_PART_CHAR_P (ch))
	    return 0;
	}
    }

  return 1;
}

void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries)
{
  unsigned int i, j;

  int saw_save_temps = 0;

  /* This will be 0 if we encounter a situation where we should not
     link in libgcj.  */
  int library = 1;

  /* This will be 1 if multiple input files (.class and/or .java)
     should be passed to a single jc1 invocation. */
  int combine_inputs = 0;

  /* Number of .java and .class source file arguments seen. */
  int java_files_count = 0;
  int class_files_count = 0;
  /* Number of .zip or .jar file arguments seen. */
  int zip_files_count = 0;
  /* Number of '@FILES' arguments seen. */
  int indirect_files_count = 0;

  /* Name of file containing list of files to compile. */
  char *filelist_filename = 0;

  FILE *filelist_file = 0;

  /* The number of arguments being added to what's in argv, other than
     libraries.  */
  int added = 2;

  /* The new argument list will be contained in this.  */
  struct cl_decoded_option *new_decoded_options;

  /* Nonzero if we saw a `-xfoo' language specification on the
     command line.  Used to avoid adding our own -xc++ if the user
     already gave a language for the file.  */
  int saw_speclang = 0;

  /* Saw --resource, -C or -o options, respectively. */
  int saw_resource = 0;
  int saw_C = 0;
  int saw_o = 0;

  /* Saw some -O* or -g* option, respectively. */
  int saw_O = 0;
  int saw_g = 0;

  /* Saw a `-D' option.  */
  int saw_D = 0;

  /* An array used to flag each argument that needs a bit set for
     LANGSPEC, MATHLIB, WITHLIBC, or GCLIB.  */
  int *args;

  /* The total number of arguments with the new stuff.  */
  unsigned int argc;

  /* The argument list.  */
  struct cl_decoded_option *decoded_options;

  /* The number of libraries added in.  */
  int added_libraries;

  /* The total number of arguments with the new stuff.  */
  unsigned int num_args = 1;

  /* Nonzero if linking is supposed to happen.  */
  int will_link = 1;

  /* Nonzero if we want to find the spec file.  */
  int want_spec_file = 1;

  /* The argument we use to specify the spec file.  */
  char *spec_file = NULL;

  /* If linking, nonzero if the BC-ABI is in use.  */
  int link_for_bc_abi = 0;

  argc = *in_decoded_options_count;
  decoded_options = *in_decoded_options;
  added_libraries = *in_added_libraries;

  args = XCNEWVEC (int, argc);

  for (i = 1; i < argc; i++)
    {
      switch (decoded_options[i].opt_index)
	{
	case OPT_nostdlib:
	case OPT_nodefaultlibs:
	  library = 0;
	  break;

	case OPT_fmain_:
	  main_class_name = decoded_options[i].arg;
	  added--;
	  break;

	case OPT__help:
	  want_spec_file = 0;
	  break;

	case OPT_v:
	  if (argc == 2)
	    {
	      /* If they only gave us `-v', don't try to link
		 in libgcj.  */ 
	      library = 0;
	    }
	  break;

	case OPT_x:
	  saw_speclang = 1;
	  break;

	case OPT_C:
	  saw_C = 1;
	  want_spec_file = 0;
	  if (library != 0)
	    added -= 2;
	  library = 0;
	  will_link = 0;
	  break;

	case OPT_fcompile_resource_:
	  saw_resource = 1;
	  want_spec_file = 0;
	  if (library != 0)
	    --added;
	  library = 0;
	  will_link = 0;
	  break;

	case OPT_D:
	  saw_D = 1;
	  break;

	case OPT_g:
	case OPT_gcoff:
	case OPT_gdwarf_:
	case OPT_ggdb:
	case OPT_gstabs:
	case OPT_gstabs_:
	case OPT_gvms:
	case OPT_gxcoff:
	case OPT_gxcoff_:
	  saw_g = 1;
	  break;

	case OPT_O:
	case OPT_Os:
	case OPT_Ofast:
	  saw_O = 1;
	  break;

	case OPT_o:
	  saw_o = 1;
	  break;

	case OPT_fclasspath_:
	case OPT_fbootclasspath_:
	case OPT_extdirs:
	  added -= 1;
	  break;

	case OPT_c:
	case OPT_S:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	  /* Don't specify libraries if we won't link, since that would
	     cause a warning.  */
	  library = 0;
	  added -= 2;

	  /* Remember this so we can confirm -fmain option.  */
	  will_link = 0;
	  break;

	case OPT_fsyntax_only:
	  library = 0;
	  will_link = 0;
	  continue;

	case OPT_save_temps:
	  saw_save_temps = 1;
	  break;

	case OPT_static_libgcc:
	case OPT_static:
	  shared_libgcc = 0;
	  break;

	case OPT_findirect_dispatch:
	  link_for_bc_abi = 1;
	  break;

	case OPT_SPECIAL_input_file:
	  {
	    const char *arg = decoded_options[i].arg;
	    int len;

	    /* We don't do this anymore, since we don't get them with minus
	       signs on them.  */
	    if (arg[0] == '\0' || arg[1] == '\0')
	      continue;

	    if (saw_speclang)
	      {
		saw_speclang = 0;
		continue;
	      }

	    if (saw_resource)
	      {
		args[i] |= RESOURCE_FILE_ARG;
		added += 2;  /* for -xjava and -xnone */
	      }

	    if (arg[0] == '@')
	      {
		args[i] |= INDIRECT_FILE_ARG;
		indirect_files_count++;
		added += 2;  /* for -xjava and -xnone */
	      }

	    len = strlen (arg);
	    if (len > 5 && strcmp (arg + len - 5, ".java") == 0)
	      {
		args[i] |= JAVA_FILE_ARG;
		java_files_count++;
	      }
	    if (len > 6 && strcmp (arg + len - 6, ".class") == 0)
	      {
		args[i] |= CLASS_FILE_ARG;
		class_files_count++;
	      }
	    if (len > 4
		&& (strcmp (arg + len - 4, ".zip") == 0
		    || strcmp (arg + len - 4, ".jar") == 0))
	      {
		args[i] |= ZIP_FILE_ARG;
		zip_files_count++;
	      }
	  }

	default:
	  /* Pass other options through.  */
	  continue;
	}
    }

  if (saw_D && ! main_class_name)
    fatal_error ("can%'t specify %<-D%> without %<--main%>");

  if (main_class_name && ! verify_class_name (main_class_name))
    fatal_error ("%qs is not a valid class name", main_class_name);

  num_args = argc + added;
  if (saw_resource)
    {
      if (! saw_o)
	fatal_error ("--resource requires -o");
    }
  if (saw_C)
    {
      num_args += 3;
      if (class_files_count + zip_files_count > 0)
	{
	  warning (0, "already-compiled .class files ignored with -C"); 
	  num_args -= class_files_count + zip_files_count;
	  class_files_count = 0;
	  zip_files_count = 0;
	}
      num_args += 2;  /* For -o NONE. */
      if (saw_o)
	fatal_error ("cannot specify both -C and -o");
    }
  if ((saw_o && java_files_count + class_files_count + zip_files_count > 1)
      || (saw_C && java_files_count > 1)
      || (indirect_files_count > 0
	  && java_files_count + class_files_count + zip_files_count > 0))
    combine_inputs = 1;

  if (combine_inputs)
    {
      filelist_filename = make_temp_file ("jx");
      if (filelist_filename == NULL)
	fatal_error ("cannot create temporary file");
      record_temp_file (filelist_filename, ! saw_save_temps, 0);
      filelist_file = fopen (filelist_filename, "w");
      if (filelist_file == NULL)
	pfatal_with_name (filelist_filename);
      num_args -= java_files_count + class_files_count + zip_files_count;
      num_args += 3;  /* for the combined arg "-xjava", and "-xnone" */
    }

  if (main_class_name)
    {
      lang_specific_extra_outfiles++;
    }
  if (saw_g + saw_O == 0)
    num_args++;
  num_args++;
  /* An additional entry for the classpath.  */
  num_args++;

  if (combine_inputs || indirect_files_count > 0)
    num_args += 1; /* for "-ffilelist-file" */
  if (combine_inputs && indirect_files_count > 0)
    fatal_error ("using both @FILE with multiple files not implemented");

  /* There's no point adding -shared-libgcc if we don't have a shared
     libgcc.  */
#ifndef ENABLE_SHARED_LIBGCC
  shared_libgcc = 0;
#endif  
  
  if (java_files_count > 0)
    ++num_args;

  num_args += shared_libgcc;

  num_args += link_for_bc_abi;

  new_decoded_options = XNEWVEC (struct cl_decoded_option, num_args);
  j = 0;

  new_decoded_options[j++] = decoded_options[0];

  if (combine_inputs || indirect_files_count > 0)
    generate_option (OPT_ffilelist_file, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  if (combine_inputs)
    {
      generate_option (OPT_x, "java", 1, CL_DRIVER,
		       &new_decoded_options[j++]);
      generate_option_input_file (filelist_filename,
				  &new_decoded_options[j++]);
      generate_option (OPT_x, "none", 1, CL_DRIVER,
		       &new_decoded_options[j++]);
    }

  if (java_files_count > 0)
    generate_option (OPT_fsaw_java_file, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  jcf_path_init ();
  for (i = 1; i < argc; i++, j++)
    {
      new_decoded_options[j] = decoded_options[i];

      if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
	continue;

      if ((args[i] & RESOURCE_FILE_ARG) != 0)
	{
	  generate_option (OPT_x, "java", 1, CL_DRIVER,
			   &new_decoded_options[j++]);
	  new_decoded_options[j++] = decoded_options[i];
	  generate_option (OPT_x, "none", 1, CL_DRIVER,
			   &new_decoded_options[j]);
	}

      switch (decoded_options[i].opt_index)
	{
	case OPT_I:
	  jcf_path_include_arg (decoded_options[i].arg);
	  --j;
	  continue;

	case OPT_fclasspath_:
	  jcf_path_classpath_arg (decoded_options[i].arg);
	  --j;
	  continue;

	case OPT_fbootclasspath_:
	  jcf_path_bootclasspath_arg (decoded_options[i].arg);
	  --j;
	  continue;

	case OPT_extdirs:
	  jcf_path_extdirs_arg (decoded_options[i].arg);
	  --j;
	  continue;

	case OPT_L:
	  if (spec_file == NULL)
	    spec_file = find_spec_file (decoded_options[i].arg);
	  break;

	case OPT_fmain_:
	  if (! will_link)
	    fatal_error ("cannot specify %<main%> class when not linking");
	  --j;
	  continue;
	}

      if ((args[i] & INDIRECT_FILE_ARG) != 0)
	{
	  generate_option (OPT_x, "java", 1, CL_DRIVER,
			   &new_decoded_options[j++]);
	  /* Drop '@'.  */
	  generate_option_input_file (decoded_options[i].arg + 1,
				      &new_decoded_options[j++]);
	  generate_option (OPT_x, "none", 1, CL_DRIVER,
			   &new_decoded_options[j]);
	}

      if ((args[i] & (CLASS_FILE_ARG|ZIP_FILE_ARG)) && saw_C)
	{
	  --j;
	  continue;
	}

      if (combine_inputs
	  && (args[i] & (CLASS_FILE_ARG|JAVA_FILE_ARG|ZIP_FILE_ARG)) != 0)
	{
	  fputs (decoded_options[i].arg, filelist_file);
	  fputc ('\n', filelist_file);
	  --j;
	  continue;
	}
  }

  /* Handle classpath setting.  We specify the bootclasspath since
     that requires the fewest changes to our existing code...  */
  jcf_path_seal (0);
  generate_option (OPT_fbootclasspath_, jcf_path_compute (""), 1,
		   CL_DRIVER, &new_decoded_options[j++]);

  if (combine_inputs)
    {
      if (fclose (filelist_file))
	pfatal_with_name (filelist_filename);
    }

  /* If we saw no -O or -g option, default to -g1, for javac compatibility. */
  if (saw_g + saw_O == 0)
    generate_option (OPT_g, "1", 1, CL_DRIVER, &new_decoded_options[j++]);

  /* Read the specs file corresponding to libgcj.
     If we didn't find the spec file on the -L path, then we hope it
     is somewhere in the standard install areas.  */
  if (want_spec_file)
    generate_option (OPT_specs_, spec_file == NULL ? "libgcj.spec" : spec_file,
		     1, CL_DRIVER, &new_decoded_options[j++]);

  if (saw_C)
    {
      generate_option (OPT_fsyntax_only, NULL, 1, CL_DRIVER,
		       &new_decoded_options[j++]);
      generate_option (OPT_femit_class_files, NULL, 1, CL_DRIVER,
		       &new_decoded_options[j++]);
      generate_option (OPT_S, NULL, 1, CL_DRIVER, &new_decoded_options[j++]);
      generate_option (OPT_o, "NONE", 1, CL_DRIVER,
		       &new_decoded_options[j++]);
    }
  
  if (shared_libgcc)
    generate_option (OPT_shared_libgcc, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  if (link_for_bc_abi)
    generate_option (OPT_s_bc_abi, NULL, 1, CL_DRIVER,
		     &new_decoded_options[j++]);

  *in_decoded_options_count = j;
  *in_decoded_options = new_decoded_options;
  *in_added_libraries = added_libraries;
}

int
lang_specific_pre_link (void)
{
  int err;
  if (main_class_name == NULL)
    return 0;
  /* Append `main' to make the filename unique and allow

	gcj --main=hello -save-temps hello.java

     to work.  jvgenmain needs to strip this `main' to arrive at the correct
     class name.  Append dummy `.c' that can be stripped by set_input so %b
     is correct.  */ 
  set_input (concat (main_class_name, "main.c", NULL));
  err = do_spec (jvgenmain_spec);
  if (err == 0)
    {
      /* Shift the outfiles array so the generated main comes first.
	 This is important when linking against (non-shared) libraries,
	 since otherwise we risk (a) nothing getting linked or
	 (b) 'main' getting picked up from a library. */
      int i = n_infiles;
      const char *generated = outfiles[i];
      while (--i >= 0)
	outfiles[i + 1] = outfiles[i];
      outfiles[0] = generated;
    }
  return err;
}
