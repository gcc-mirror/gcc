/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "timevar.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs-libfuncs.h"
#include "insn-config.h"
#include "ira.h"
#include "recog.h"
#include "cgraph.h"
#include "coverage.h"
#include "diagnostic.h"
#include "varasm.h"
#include "tree-inline.h"
#include "realmpfr.h"	/* For GMP/MPFR/MPC versions, in print_version.  */
#include "version.h"
#include "flags.h"
#include "insn-attr.h"
#include "output.h"
#include "toplev.h"
#include "expr.h"
#include "intl.h"
#include "tree-diagnostic.h"
#include "reload.h"
#include "lra.h"
#include "dwarf2asm.h"
#include "debug.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "cfgloop.h" /* for init_set_costs */
#include "hosthooks.h"
#include "opts.h"
#include "opts-diagnostic.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "tsan.h"
#include "plugin.h"
#include "context.h"
#include "pass_manager.h"
#include "auto-profile.h"
#include "dwarf2out.h"
#include "ipa-reference.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-utils.h"
#include "gcse.h"
#include "omp-offload.h"
#include "edit-context.h"
#include "tree-pass.h"
#include "dumpfile.h"
#include "ipa-fnsummary.h"
#include "dump-context.h"
#include "print-tree.h"
#include "optinfo-emit-json.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "ipa-param-manipulation.h"
#include "dbgcnt.h"
#include "gcc-urlifier.h"

#include "selftest.h"

#ifdef HAVE_isl
#include <isl/version.h>
#endif

static void general_init (const char *, bool);
static void backend_init (void);
static int lang_dependent_init (const char *);
static void init_asm_output (const char *);
static void finalize ();

static void crash_signal (int) ATTRIBUTE_NORETURN;
static void compile_file (void);

/* Decoded options, and number of such options.  */
struct cl_decoded_option *save_decoded_options;
unsigned int save_decoded_options_count;

/* Vector of saved Optimization decoded command line options.  */
vec<cl_decoded_option> *save_opt_decoded_options;

/* Debug hooks - dependent upon command line options.  */

const struct gcc_debug_hooks *debug_hooks;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to the FUNC_BEGIN label of the current function, or NULL
   if none.  */
const char * current_function_func_begin_label;

/* A random sequence of characters, unless overridden by user.  */
static const char *flag_random_seed;

/* A local time stamp derived from the time of compilation. It will be
   zero if the system cannot provide a time.  It will be -1u, if the
   user has specified a particular random seed.  */
unsigned local_tick;

/* Random number for this compilation */
HOST_WIDE_INT random_seed;

/* -f flags.  */

/* When non-NULL, indicates that whenever space is allocated on the
   stack, the resulting stack pointer must not pass this
   address---that is, for stacks that grow downward, the stack pointer
   must always be greater than or equal to this address; for stacks
   that grow upward, the stack pointer must be less than this address.
   At present, the rtx may be either a REG or a SYMBOL_REF, although
   the support provided depends on the backend.  */
rtx stack_limit_rtx;

class target_flag_state default_target_flag_state;
#if SWITCHABLE_TARGET
class target_flag_state *this_target_flag_state = &default_target_flag_state;
#else
#define this_target_flag_state (&default_target_flag_state)
#endif

/* The user symbol prefix after having resolved same.  */
const char *user_label_prefix;

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *aux_info_file;
FILE *callgraph_info_file = NULL;
static bitmap callgraph_info_external_printed;
FILE *stack_usage_file = NULL;
static bool no_backend = false;

/* The current working directory of a translation.  It's generally the
   directory from which compilation was initiated, but a preprocessed
   file may specify the original directory in which it was
   created.  */

static const char *src_pwd;

/* Initialize src_pwd with the given string, and return true.  If it
   was already initialized, return false.  As a special case, it may
   be called with a NULL argument to test whether src_pwd has NOT been
   initialized yet.  */

bool
set_src_pwd (const char *pwd)
{
  if (src_pwd)
    {
      if (strcmp (src_pwd, pwd) == 0)
	return true;
      else
	return false;
    }

  src_pwd = xstrdup (pwd);
  return true;
}

/* Return the directory from which the translation unit was initiated,
   in case set_src_pwd() was not called before to assign it a
   different value.  */

const char *
get_src_pwd (void)
{
  if (! src_pwd)
    {
      src_pwd = getpwd ();
      if (!src_pwd)
	src_pwd = ".";
    }

   return src_pwd;
}

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */
void
announce_function (tree decl)
{
  if (!quiet_flag)
    {
      if (rtl_dump_and_exit)
	fprintf (stderr, "%s ",
		 identifier_to_locale (IDENTIFIER_POINTER (DECL_NAME (decl))));
      else
	fprintf (stderr, " %s",
		 identifier_to_locale (lang_hooks.decl_printable_name (decl, 2)));
      fflush (stderr);
      pp_needs_newline (global_dc->printer) = true;
      diagnostic_set_last_function (global_dc, (diagnostic_info *) NULL);
    }
}

/* Initialize local_tick with the time of day, or -1 if
   flag_random_seed is set.  */

static void
init_local_tick (void)
{
  if (!flag_random_seed)
    {
#ifdef HAVE_GETTIMEOFDAY
      {
	struct timeval tv;

	gettimeofday (&tv, NULL);
	local_tick = (unsigned) tv.tv_sec * 1000 + tv.tv_usec / 1000;
      }
#else
      {
	time_t now = time (NULL);

	if (now != (time_t)-1)
	  local_tick = (unsigned) now;
      }
#endif
    }
  else
    local_tick = -1;
}

/* Obtain the random_seed.  Unless NOINIT, initialize it if
   it's not provided in the command line.  */

HOST_WIDE_INT
get_random_seed (bool noinit)
{
  if (!random_seed && !noinit)
    {
      int fd = open ("/dev/urandom", O_RDONLY);
      if (fd >= 0)
        {
          if (read (fd, &random_seed, sizeof (random_seed))
              != sizeof (random_seed))
            random_seed = 0;
          close (fd);
        }
      if (!random_seed)
	random_seed = local_tick ^ getpid ();
    }
  return random_seed;
}

/* Set flag_random_seed to VAL, and if non-null, reinitialize random_seed.  */

void
set_random_seed (const char *val)
{
  flag_random_seed = val;
  if (flag_random_seed)
    {
      char *endp;

      /* When the driver passed in a hex number don't crc it again */
      random_seed = strtoul (flag_random_seed, &endp, 0);
      if (!(endp > flag_random_seed && *endp == 0))
        random_seed = crc32_string (0, flag_random_seed);
    }
}

/* Handler for fatal signals, such as SIGSEGV.  These are transformed
   into ICE messages, which is much more user friendly.  In case the
   error printer crashes, reset the signal to prevent infinite recursion.  */

static void
crash_signal (int signo)
{
  signal (signo, SIG_DFL);

  /* If we crashed while processing an ASM statement, then be a little more
     graceful.  It's most likely the user's fault.  */
  if (this_is_asm_operands)
    {
      output_operand_lossage ("unrecoverable error");
      exit (FATAL_EXIT_CODE);
    }

  internal_error ("%s", strsignal (signo));
}

/* A subroutine of wrapup_global_declarations.  We've come to the end of
   the compilation unit.  All deferred variables should be undeferred,
   and all incomplete decls should be finalized.  */

void
wrapup_global_declaration_1 (tree decl)
{
  /* We're not deferring this any longer.  Assignment is conditional to
     avoid needlessly dirtying PCH pages.  */
  if (HAS_DECL_ASSEMBLER_NAME_P (decl)
      && DECL_DEFER_OUTPUT (decl) != 0)
    DECL_DEFER_OUTPUT (decl) = 0;

  if (VAR_P (decl) && DECL_SIZE (decl) == 0)
    lang_hooks.finish_incomplete_decl (decl);
}

/* A subroutine of wrapup_global_declarations.  Decide whether or not DECL
   needs to be output.  Return true if it is output.  */

bool
wrapup_global_declaration_2 (tree decl)
{
  if (TREE_ASM_WRITTEN (decl) || DECL_EXTERNAL (decl)
      || (VAR_P (decl) && DECL_HAS_VALUE_EXPR_P (decl)))
    return false;

  /* Don't write out static consts, unless we still need them.

     We also keep static consts if not optimizing (for debugging),
     unless the user specified -fno-keep-static-consts.
     ??? They might be better written into the debug information.
     This is possible when using DWARF.

     A language processor that wants static constants to be always
     written out (even if it is not used) is responsible for
     calling rest_of_decl_compilation itself.  E.g. the C front-end
     calls rest_of_decl_compilation from finish_decl.
     One motivation for this is that is conventional in some
     environments to write things like:
     static const char rcsid[] = "... version string ...";
     intending to force the string to be in the executable.

     A language processor that would prefer to have unneeded
     static constants "optimized away" would just defer writing
     them out until here.  E.g. C++ does this, because static
     constants are often defined in header files.

     ??? A tempting alternative (for both C and C++) would be
     to force a constant to be written if and only if it is
     defined in a main file, as opposed to an include file.  */

  if (VAR_P (decl) && TREE_STATIC (decl))
    {
      varpool_node *node;
      bool needed = true;
      node = varpool_node::get (decl);

      if (!node && flag_ltrans)
	needed = false;
      else if (node && node->definition)
	needed = false;
      else if (node && node->alias)
	needed = false;
      else if (!symtab->global_info_ready
	       && (TREE_USED (decl)
		   || TREE_USED (DECL_ASSEMBLER_NAME (decl))))
	/* needed */;
      else if (node && node->analyzed)
	/* needed */;
      else if (DECL_COMDAT (decl))
	needed = false;
      else if (TREE_READONLY (decl) && !TREE_PUBLIC (decl)
	       && (optimize || !flag_keep_static_consts
		   || DECL_ARTIFICIAL (decl)))
	needed = false;

      if (needed)
	{
	  rest_of_decl_compilation (decl, 1, 1);
	  return true;
	}
    }

  return false;
}

/* Do any final processing required for the declarations in VEC, of
   which there are LEN.  We write out inline functions and variables
   that have been deferred until this point, but which are required.
   Returns nonzero if anything was put out.  */

bool
wrapup_global_declarations (tree *vec, int len)
{
  bool reconsider, output_something = false;
  int i;

  for (i = 0; i < len; i++)
    wrapup_global_declaration_1 (vec[i]);

  /* Now emit any global variables or functions that we have been
     putting off.  We need to loop in case one of the things emitted
     here references another one which comes earlier in the list.  */
  do
    {
      reconsider = false;
      for (i = 0; i < len; i++)
	reconsider |= wrapup_global_declaration_2 (vec[i]);
      if (reconsider)
	output_something = true;
    }
  while (reconsider);

  return output_something;
}

/* Compile an entire translation unit.  Write a file of assembly
   output and various debugging dumps.  */

static void
compile_file (void)
{
  timevar_start (TV_PHASE_PARSING);
  timevar_push (TV_PARSE_GLOBAL);

  /* Parse entire file and generate initial debug information.  */
  lang_hooks.parse_file ();

  timevar_pop (TV_PARSE_GLOBAL);
  timevar_stop (TV_PHASE_PARSING);

  if (flag_dump_locations)
    dump_location_info (stderr);

  free_attr_data ();

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */

  if (flag_syntax_only || flag_wpa)
    return;
 
  /* Reset maximum_field_alignment, it can be adjusted by #pragma pack
     and this shouldn't influence any types built by the middle-end
     from now on (like gcov_info_type).  */
  maximum_field_alignment = initial_max_fld_align * BITS_PER_UNIT;

  ggc_protect_identifiers = false;

  /* Run the actual compilation process.  */
  if (!in_lto_p)
    {
      timevar_start (TV_PHASE_OPT_GEN);
      symtab->finalize_compilation_unit ();
      timevar_stop (TV_PHASE_OPT_GEN);
    }

  /* Perform any post compilation-proper parser cleanups and
     processing.  This is currently only needed for the C++ parser,
     which can be hopefully cleaned up so this hook is no longer
     necessary.  */
  if (lang_hooks.decls.post_compilation_parsing_cleanups)
    lang_hooks.decls.post_compilation_parsing_cleanups ();

  dump_context::get ().finish_any_json_writer ();

  if (seen_error ())
    return;

  timevar_start (TV_PHASE_LATE_ASM);

  /* Compilation unit is finalized.  When producing non-fat LTO object, we are
     basically finished.  */
  if ((in_lto_p && flag_incremental_link != INCREMENTAL_LINK_LTO)
      || !flag_lto || flag_fat_lto_objects)
    {
      /* File-scope initialization for AddressSanitizer.  */
      if (flag_sanitize & SANITIZE_ADDRESS)
        asan_finish_file ();

      if (flag_sanitize & SANITIZE_THREAD)
	tsan_finish_file ();

      if (gate_hwasan ())
	hwasan_finish_file ();

      omp_finish_file ();

      output_shared_constant_pool ();
      output_object_blocks ();
      finish_tm_clone_pairs ();

      /* Write out any pending weak symbol declarations.  */
      weak_finish ();

      /* This must be at the end before unwind and debug info.
	 Some target ports emit PIC setup thunks here.  */
      insn_locations_init ();
      targetm.asm_out.code_end ();

      /* Do dbx symbols.  */
      timevar_push (TV_SYMOUT);

#if defined DWARF2_DEBUGGING_INFO || defined DWARF2_UNWIND_INFO
      dwarf2out_frame_finish ();
#endif

      debuginfo_start ();
      (*debug_hooks->finish) (main_input_filename);
      debuginfo_stop ();
      timevar_pop (TV_SYMOUT);

      /* Output some stuff at end of file if nec.  */

      dw2_output_indirect_constants ();

      /* Flush any pending external directives.  */
      process_pending_assemble_externals ();
   }

  /* Let linker plugin know that this is a slim object and must be LTOed
     even when user did not ask for it.  */
  if (flag_generate_lto && !flag_fat_lto_objects)
    {
#if defined ASM_OUTPUT_ALIGNED_DECL_COMMON
      ASM_OUTPUT_ALIGNED_DECL_COMMON (asm_out_file, NULL_TREE, "__gnu_lto_slim",
				      HOST_WIDE_INT_1U, 8);
#elif defined ASM_OUTPUT_ALIGNED_COMMON
      ASM_OUTPUT_ALIGNED_COMMON (asm_out_file, "__gnu_lto_slim",
				 HOST_WIDE_INT_1U, 8);
#else
      ASM_OUTPUT_COMMON (asm_out_file, "__gnu_lto_slim",
			 HOST_WIDE_INT_1U,
			 HOST_WIDE_INT_1U);
#endif
    }

  /* Attach a special .ident directive to the end of the file to identify
     the version of GCC which compiled this code.  The format of the .ident
     string is patterned after the ones produced by native SVR4 compilers.  */
  if (!flag_no_ident)
    {
      const char *pkg_version = "(GNU) ";
      char *ident_str;

      if (strcmp ("(GCC) ", pkgversion_string))
	pkg_version = pkgversion_string;

      ident_str = ACONCAT (("GCC: ", pkg_version, version_string, NULL));
      targetm.asm_out.output_ident (ident_str);
    }

  /* Auto profile finalization. */
  if (flag_auto_profile)
    end_auto_profile ();

  /* Invoke registered plugin callbacks.  */
  invoke_plugin_callbacks (PLUGIN_FINISH_UNIT, NULL);

  /* This must be at the end.  Some target ports emit end of file directives
     into the assembly file here, and hence we cannot output anything to the
     assembly file after this point.  */
  targetm.asm_out.file_end ();

  timevar_stop (TV_PHASE_LATE_ASM);
}

/* Print version information to FILE.
   Each line begins with INDENT (for the case where FILE is the
   assembler output file).

   If SHOW_GLOBAL_STATE is true (for cc1 etc), we are within the compiler
   proper and can print pertinent state (e.g. params and plugins).

   If SHOW_GLOBAL_STATE is false (for use by libgccjit), we are outside the
   compiler, and we don't hold the mutex on the compiler's global state:
   we can't print params and plugins, since they might not be initialized,
   or might be being manipulated by a compile running in another
   thread.  */

void
print_version (FILE *file, const char *indent, bool show_global_state)
{
  static const char fmt1[] =
#ifdef __GNUC__
    N_("%s%s%s %sversion %s (%s)\n%s\tcompiled by GNU C version %s, ")
#else
    N_("%s%s%s %sversion %s (%s) compiled by CC, ")
#endif
    ;
  static const char fmt2[] =
    N_("GMP version %s, MPFR version %s, MPC version %s, isl version %s\n");
  static const char fmt3[] =
    N_("%s%swarning: %s header version %s differs from library version %s.\n");
  static const char fmt4[] =
    N_("%s%sGGC heuristics: --param ggc-min-expand=%d --param ggc-min-heapsize=%d\n");
#ifndef __VERSION__
#define __VERSION__ "[?]"
#endif
  fprintf (file,
	   file == stderr ? _(fmt1) : fmt1,
	   indent, *indent != 0 ? " " : "",
	   lang_hooks.name, pkgversion_string, version_string, TARGET_NAME,
	   indent, __VERSION__);

  /* We need to stringify the GMP macro values.  Ugh, gmp_version has
     two string formats, "i.j.k" and "i.j" when k is zero.  As of
     gmp-4.3.0, GMP always uses the 3 number format.  */
#define GCC_GMP_STRINGIFY_VERSION3(X) #X
#define GCC_GMP_STRINGIFY_VERSION2(X) GCC_GMP_STRINGIFY_VERSION3 (X)
#define GCC_GMP_VERSION_NUM(X,Y,Z) (((X) << 16L) | ((Y) << 8) | (Z))
#define GCC_GMP_VERSION \
  GCC_GMP_VERSION_NUM(__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, __GNU_MP_VERSION_PATCHLEVEL)
#if GCC_GMP_VERSION < GCC_GMP_VERSION_NUM(4,3,0) && __GNU_MP_VERSION_PATCHLEVEL == 0
#define GCC_GMP_STRINGIFY_VERSION \
  GCC_GMP_STRINGIFY_VERSION2 (__GNU_MP_VERSION) "." \
  GCC_GMP_STRINGIFY_VERSION2 (__GNU_MP_VERSION_MINOR)
#else
#define GCC_GMP_STRINGIFY_VERSION \
  GCC_GMP_STRINGIFY_VERSION2 (__GNU_MP_VERSION) "." \
  GCC_GMP_STRINGIFY_VERSION2 (__GNU_MP_VERSION_MINOR) "." \
  GCC_GMP_STRINGIFY_VERSION2 (__GNU_MP_VERSION_PATCHLEVEL)
#endif
  fprintf (file,
	   file == stderr ? _(fmt2) : fmt2,
	   GCC_GMP_STRINGIFY_VERSION, MPFR_VERSION_STRING, MPC_VERSION_STRING,
#ifndef HAVE_isl
	   "none"
#else
	   isl_version ()
#endif
	   );
  if (strcmp (GCC_GMP_STRINGIFY_VERSION, gmp_version))
    fprintf (file,
	     file == stderr ? _(fmt3) : fmt3,
	     indent, *indent != 0 ? " " : "",
	     "GMP", GCC_GMP_STRINGIFY_VERSION, gmp_version);
  if (strcmp (MPFR_VERSION_STRING, mpfr_get_version ()))
    fprintf (file,
	     file == stderr ? _(fmt3) : fmt3,
	     indent, *indent != 0 ? " " : "",
	     "MPFR", MPFR_VERSION_STRING, mpfr_get_version ());
  if (strcmp (MPC_VERSION_STRING, mpc_get_version ()))
    fprintf (file,
	     file == stderr ? _(fmt3) : fmt3,
	     indent, *indent != 0 ? " " : "",
	     "MPC", MPC_VERSION_STRING, mpc_get_version ());

  if (show_global_state)
    {
      fprintf (file,
	       file == stderr ? _(fmt4) : fmt4,
	       indent, *indent != 0 ? " " : "",
	       param_ggc_min_expand, param_ggc_min_heapsize);

      print_plugins_versions (file, indent);
    }
}



/* Open assembly code output file.  Do this even if -fsyntax-only is
   on, because then the driver will have provided the name of a
   temporary file or bit bucket for us.  NAME is the file specified on
   the command line, possibly NULL.  */
static void
init_asm_output (const char *name)
{
  if (name == NULL && asm_file_name == 0)
    asm_out_file = stdout;
  else
    {
      if (asm_file_name == 0)
	{
	  int len = strlen (dump_base_name);
	  char *dumpname = XNEWVEC (char, len + 6);

	  memcpy (dumpname, dump_base_name, len + 1);
	  strip_off_ending (dumpname, len);
	  strcat (dumpname, ".s");
	  asm_file_name = dumpname;
	}
      if (!strcmp (asm_file_name, "-"))
	asm_out_file = stdout;
      else if (!canonical_filename_eq (asm_file_name, name)
	       || !strcmp (asm_file_name, HOST_BIT_BUCKET))
	asm_out_file = fopen (asm_file_name, "w");
      else
	/* Use UNKOWN_LOCATION to prevent gcc from printing the first
	   line in the current file. */
	fatal_error (UNKNOWN_LOCATION,
		     "input file %qs is the same as output file",
		     asm_file_name);
      if (asm_out_file == 0)
	fatal_error (UNKNOWN_LOCATION,
		     "cannot open %qs for writing: %m", asm_file_name);
    }

  if (!flag_syntax_only && !(global_dc->get_lang_mask () & CL_LTODump))
    {
      targetm.asm_out.file_start ();

      if (flag_record_gcc_switches)
	{
	  if (targetm.asm_out.record_gcc_switches)
	    {
	      const char *str
		= gen_producer_string (lang_hooks.name,
				       save_decoded_options,
				       save_decoded_options_count);
	      targetm.asm_out.record_gcc_switches (str);
	    }
	  else
	    inform (UNKNOWN_LOCATION,
		    "%<-frecord-gcc-switches%> is not supported by "
		    "the current target");
	}

      if (flag_verbose_asm)
	{
	  print_version (asm_out_file, ASM_COMMENT_START, true);
	  fputs (ASM_COMMENT_START, asm_out_file);
	  fputs (" options passed: ", asm_out_file);
	  char *cmdline = gen_command_line_string (save_decoded_options,
						   save_decoded_options_count);
	  fputs (cmdline, asm_out_file);
	  free (cmdline);
	  fputc ('\n', asm_out_file);
	}
    }
}

/* A helper function; used as the reallocator function for cpp's line
   table.  */
static void *
realloc_for_line_map (void *ptr, size_t len)
{
  return ggc_realloc (ptr, len);
}

/* A helper function: used as the allocator function for
   identifier_to_locale.  */
static void *
alloc_for_identifier_to_locale (size_t len)
{
  return ggc_alloc_atomic (len);
}

/* Output stack usage information.  */
static void
output_stack_usage_1 (FILE *cf)
{
  static bool warning_issued = false;
  enum stack_usage_kind_type { STATIC = 0, DYNAMIC, DYNAMIC_BOUNDED };
  const char *stack_usage_kind_str[] = {
    "static",
    "dynamic",
    "dynamic,bounded"
  };
  HOST_WIDE_INT stack_usage = current_function_static_stack_size;
  enum stack_usage_kind_type stack_usage_kind;

  if (stack_usage < 0)
    {
      if (!warning_issued)
	{
	  warning (0, "stack usage computation not supported for this target");
	  warning_issued = true;
	}
      return;
    }

  stack_usage_kind = STATIC;

  /* Add the maximum amount of space pushed onto the stack.  */
  if (maybe_ne (current_function_pushed_stack_size, 0))
    {
      HOST_WIDE_INT extra;
      if (current_function_pushed_stack_size.is_constant (&extra))
	{
	  stack_usage += extra;
	  stack_usage_kind = DYNAMIC_BOUNDED;
	}
      else
	{
	  extra = constant_lower_bound (current_function_pushed_stack_size);
	  stack_usage += extra;
	  stack_usage_kind = DYNAMIC;
	}
    }

  /* Now on to the tricky part: dynamic stack allocation.  */
  if (current_function_allocates_dynamic_stack_space)
    {
      if (stack_usage_kind != DYNAMIC)
	{
	  if (current_function_has_unbounded_dynamic_stack_size)
	    stack_usage_kind = DYNAMIC;
	  else
	    stack_usage_kind = DYNAMIC_BOUNDED;
	}

      /* Add the size even in the unbounded case, this can't hurt.  */
      stack_usage += current_function_dynamic_stack_size;
    }

  if (cf && flag_callgraph_info & CALLGRAPH_INFO_STACK_USAGE)
    fprintf (cf, "\\n" HOST_WIDE_INT_PRINT_DEC " bytes (%s)",
	     stack_usage,
	     stack_usage_kind_str[stack_usage_kind]);

  if (stack_usage_file)
    {
      print_decl_identifier (stack_usage_file, current_function_decl,
			     PRINT_DECL_ORIGIN | PRINT_DECL_NAME
			     | PRINT_DECL_REMAP_DEBUG);
      fprintf (stack_usage_file, "\t" HOST_WIDE_INT_PRINT_DEC"\t%s\n",
	       stack_usage, stack_usage_kind_str[stack_usage_kind]);
    }

  if (warn_stack_usage >= 0 && warn_stack_usage < HOST_WIDE_INT_MAX)
    {
      const location_t loc = DECL_SOURCE_LOCATION (current_function_decl);

      if (stack_usage_kind == DYNAMIC)
	warning_at (loc, OPT_Wstack_usage_, "stack usage might be unbounded");
      else if (stack_usage > warn_stack_usage)
	{
	  if (stack_usage_kind == DYNAMIC_BOUNDED)
	    warning_at (loc,
			OPT_Wstack_usage_, "stack usage might be %wu bytes",
			stack_usage);
	  else
	    warning_at (loc, OPT_Wstack_usage_, "stack usage is %wu bytes",
			stack_usage);
	}
    }
}

/* Dump placeholder node for indirect calls in VCG format.  */

#define INDIRECT_CALL_NAME  "__indirect_call"

static void
dump_final_node_vcg_start (FILE *f, tree decl)
{
  fputs ("node: { title: \"", f);
  if (decl)
    print_decl_identifier (f, decl, PRINT_DECL_UNIQUE_NAME);
  else
    fputs (INDIRECT_CALL_NAME, f);
  fputs ("\" label: \"", f);
  if (decl)
    {
      print_decl_identifier (f, decl, PRINT_DECL_NAME);
      fputs ("\\n", f);
      print_decl_identifier (f, decl, PRINT_DECL_ORIGIN);
    }
  else
    fputs ("Indirect Call Placeholder", f);
}

/* Dump final cgraph edge in VCG format.  */

static void
dump_final_callee_vcg (FILE *f, location_t location, tree callee)
{
  if ((!callee || DECL_EXTERNAL (callee))
      && bitmap_set_bit (callgraph_info_external_printed,
			 callee ? DECL_UID (callee) + 1 : 0))
    {
      dump_final_node_vcg_start (f, callee);
      fputs ("\" shape : ellipse }\n", f);
    }

  fputs ("edge: { sourcename: \"", f);
  print_decl_identifier (f, current_function_decl, PRINT_DECL_UNIQUE_NAME);
  fputs ("\" targetname: \"", f);
  if (callee)
    print_decl_identifier (f, callee, PRINT_DECL_UNIQUE_NAME);
  else
    fputs (INDIRECT_CALL_NAME, f);
  if (LOCATION_LOCUS (location) != UNKNOWN_LOCATION)
    {
      expanded_location loc;
      fputs ("\" label: \"", f);
      loc = expand_location (location);
      fprintf (f, "%s:%d:%d", loc.file, loc.line, loc.column);
    }
  fputs ("\" }\n", f);
}

/* Dump final cgraph node in VCG format.  */

static void
dump_final_node_vcg (FILE *f)
{
  dump_final_node_vcg_start (f, current_function_decl);

  if (flag_stack_usage_info
      || (flag_callgraph_info & CALLGRAPH_INFO_STACK_USAGE))
    output_stack_usage_1 (f);

  if (flag_callgraph_info & CALLGRAPH_INFO_DYNAMIC_ALLOC)
    {
      fprintf (f, "\\n%u dynamic objects", vec_safe_length (cfun->su->dallocs));

      unsigned i;
      callinfo_dalloc *cda;
      FOR_EACH_VEC_SAFE_ELT (cfun->su->dallocs, i, cda)
	{
	  expanded_location loc = expand_location (cda->location);
	  fprintf (f, "\\n %s", cda->name);
	  fprintf (f, " %s:%d:%d", loc.file, loc.line, loc.column);
	}

      vec_free (cfun->su->dallocs);
      cfun->su->dallocs = NULL;
    }

  fputs ("\" }\n", f);

  unsigned i;
  callinfo_callee *c;
  FOR_EACH_VEC_SAFE_ELT (cfun->su->callees, i, c)
    dump_final_callee_vcg (f, c->location, c->decl);
  vec_free (cfun->su->callees);
  cfun->su->callees = NULL;
}

/* Output stack usage and callgraph info, as requested.  */
void
output_stack_usage (void)
{
  if (flag_callgraph_info)
    dump_final_node_vcg (callgraph_info_file);
  else
    output_stack_usage_1 (NULL);
}

/* Open an auxiliary output file.  */
static FILE *
open_auxiliary_file (const char *ext)
{
  char *filename;
  FILE *file;

  filename = concat (aux_base_name, ".", ext, NULL);
  file = fopen (filename, "w");
  if (!file)
    fatal_error (input_location, "cannot open %s for writing: %m", filename);
  free (filename);
  return file;
}

/* Alternative diagnostics callback for reentered ICE reporting.  */

static void
internal_error_reentered (diagnostic_context *, const char *, va_list *)
{
  /* Flush the dump file if emergency_dump_function itself caused an ICE.  */
  if (dump_file)
    fflush (dump_file);
}

/* Auxiliary callback for the diagnostics code.  */

static void
internal_error_function (diagnostic_context *, const char *, va_list *)
{
  global_dc->m_internal_error = internal_error_reentered;
  warn_if_plugins ();
  emergency_dump_function ();
}

/* Initialization of the front end environment, before command line
   options are parsed.  Signal handlers, internationalization etc.
   ARGV0 is main's argv[0].  */
static void
general_init (const char *argv0, bool init_signals)
{
  const char *p;

  p = argv0 + strlen (argv0);
  while (p != argv0 && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  hex_init ();

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  identifier_to_locale_alloc = alloc_for_identifier_to_locale;
  identifier_to_locale_free = ggc_free;

  /* Initialize the diagnostics reporting machinery, so option parsing
     can give warnings and errors.  */
  diagnostic_initialize (global_dc, N_OPTS);
  /* Set a default printer.  Language specific initializations will
     override it later.  */
  tree_diagnostics_defaults (global_dc);

  global_dc->m_source_printing.enabled
    = global_options_init.x_flag_diagnostics_show_caret;
  global_dc->m_source_printing.show_labels_p
    = global_options_init.x_flag_diagnostics_show_labels;
  global_dc->m_source_printing.show_line_numbers_p
    = global_options_init.x_flag_diagnostics_show_line_numbers;
  global_dc->set_show_cwe (global_options_init.x_flag_diagnostics_show_cwe);
  global_dc->set_show_rules (global_options_init.x_flag_diagnostics_show_rules);
  global_dc->set_path_format
    ((enum diagnostic_path_format)
     global_options_init.x_flag_diagnostics_path_format);
  global_dc->set_show_path_depths
    (global_options_init.x_flag_diagnostics_show_path_depths);
  global_dc->set_show_option_requested
    (global_options_init.x_flag_diagnostics_show_option);
  global_dc->m_source_printing.min_margin_width
    = global_options_init.x_diagnostics_minimum_margin_width;
  global_dc->m_show_column
    = global_options_init.x_flag_show_column;
  global_dc->m_internal_error = internal_error_function;
  const unsigned lang_mask = lang_hooks.option_lang_mask ();
  global_dc->set_option_hooks (option_enabled,
			       &global_options,
			       option_name,
			       get_option_url,
			       lang_mask);
  global_dc->set_urlifier (make_gcc_urlifier (lang_mask));

  if (init_signals)
    {
      /* Trap fatal signals, e.g. SIGSEGV, and convert them to ICE messages.  */
#ifdef SIGSEGV
      signal (SIGSEGV, crash_signal);
#endif
#ifdef SIGILL
      signal (SIGILL, crash_signal);
#endif
#ifdef SIGBUS
      signal (SIGBUS, crash_signal);
#endif
#ifdef SIGABRT
      signal (SIGABRT, crash_signal);
#endif
#if defined SIGIOT && (!defined SIGABRT || SIGABRT != SIGIOT)
      signal (SIGIOT, crash_signal);
#endif
#ifdef SIGFPE
      signal (SIGFPE, crash_signal);
#endif

      /* Other host-specific signal setup.  */
      (*host_hooks.extra_signals)();
  }

  /* Initialize the garbage-collector, string pools and tree type hash
     table.  */
  init_ggc ();
  init_stringpool ();
  input_location = UNKNOWN_LOCATION;
  line_table = ggc_alloc<line_maps> ();
  linemap_init (line_table, BUILTINS_LOCATION);
  line_table->m_reallocator = realloc_for_line_map;
  line_table->m_round_alloc_size = ggc_round_alloc_size;
  line_table->default_range_bits = 5;
  init_ttree ();

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  /* Create the singleton holder for global state.  This creates the
     dump manager.  */
  g = new gcc::context ();

  /* Allow languages and middle-end to register their dumps before the
     optimization passes.  */
  g->get_dumps ()->register_dumps ();

  /* Create the passes.  */
  g->set_passes (new gcc::pass_manager (g));

  symtab = new (ggc_alloc <symbol_table> ()) symbol_table ();

  statistics_early_init ();
  debuginfo_early_init ();
}

/* Return true if the current target supports -fsection-anchors.  */

static bool
target_supports_section_anchors_p (void)
{
  if (targetm.min_anchor_offset == 0 && targetm.max_anchor_offset == 0)
    return false;

  if (targetm.asm_out.output_anchor == NULL)
    return false;

  return true;
}

/* Parse "N[:M][:...]" into struct align_flags A.
   VALUES contains parsed values (in reverse order), all processed
   values are popped.  */

static void
read_log_maxskip (auto_vec<unsigned> &values, align_flags_tuple *a)
{
  unsigned n = values.pop ();
  if (n != 0)
    a->log = floor_log2 (n * 2 - 1);

  if (values.is_empty ())
    a->maxskip = n ? n - 1 : 0;
  else
    {
      unsigned m = values.pop ();
      /* -falign-foo=N:M means M-1 max bytes of padding, not M.  */
      if (m > 0)
	m--;
      a->maxskip = m;
    }

  /* Normalize the tuple.  */
  a->normalize ();
}

/* Parse "N[:M[:N2[:M2]]]" string FLAG into a pair of struct align_flags.  */

static void
parse_N_M (const char *flag, align_flags &a)
{
  if (flag)
    {
      static hash_map <nofree_string_hash, align_flags> cache;
      align_flags *entry = cache.get (flag);
      if (entry)
	{
	  a = *entry;
	  return;
	}

      auto_vec<unsigned> result_values;
      bool r = parse_and_check_align_values (flag, NULL, result_values, false,
					     UNKNOWN_LOCATION);
      if (!r)
	return;

      /* Reverse values for easier manipulation.  */
      result_values.reverse ();

      read_log_maxskip (result_values, &a.levels[0]);
      if (!result_values.is_empty ())
	read_log_maxskip (result_values, &a.levels[1]);
#ifdef SUBALIGN_LOG
      else
	{
	  /* N2[:M2] is not specified.  This arch has a default for N2.
	     Before -falign-foo=N:M:N2:M2 was introduced, x86 had a tweak.
	     -falign-functions=N with N > 8 was adding secondary alignment.
	     -falign-functions=10 was emitting this before every function:
			.p2align 4,,9
			.p2align 3
	     Now this behavior (and more) can be explicitly requested:
	     -falign-functions=16:10:8
	     Retain old behavior if N2 is missing: */

	  int align = 1 << a.levels[0].log;
	  int subalign = 1 << SUBALIGN_LOG;

	  if (a.levels[0].log > SUBALIGN_LOG
	      && a.levels[0].maxskip >= subalign - 1)
	    {
	      /* Set N2 unless subalign can never have any effect.  */
	      if (align > a.levels[0].maxskip + 1)
		{
		  a.levels[1].log = SUBALIGN_LOG;
		  a.levels[1].normalize ();
		}
	    }
	}
#endif

      /* Cache seen value.  */
      cache.put (flag, a);
    }
}

/* Process -falign-foo=N[:M[:N2[:M2]]] options.  */

void
parse_alignment_opts (void)
{
  parse_N_M (str_align_loops, align_loops);
  parse_N_M (str_align_jumps, align_jumps);
  parse_N_M (str_align_labels, align_labels);
  parse_N_M (str_align_functions, align_functions);
}

/* Process the options that have been parsed.  */
static void
process_options ()
{
  const char *language_string = lang_hooks.name;

  maximum_field_alignment = initial_max_fld_align * BITS_PER_UNIT;

  /* Some machines may reject certain combinations of options.  */
  location_t saved_location = input_location;
  input_location = UNKNOWN_LOCATION;
  targetm.target_option.override ();
  input_location = saved_location;

  if (flag_diagnostics_generate_patch)
    global_dc->create_edit_context ();

  /* Avoid any informative notes in the second run of -fcompare-debug.  */
  if (flag_compare_debug) 
    diagnostic_inhibit_notes (global_dc);

  if (flag_section_anchors && !target_supports_section_anchors_p ())
    {
      warning_at (UNKNOWN_LOCATION, OPT_fsection_anchors,
		  "this target does not support %qs",
		  "-fsection-anchors");
      flag_section_anchors = 0;
    }

  if (flag_short_enums == 2)
    flag_short_enums = targetm.default_short_enums ();

  /* Set aux_base_name if not already set.  */
  if (aux_base_name)
    ;
  else if (dump_base_name)
    {
      const char *name = dump_base_name;
      int nlen, len;

      if (dump_base_ext && (len = strlen (dump_base_ext))
	  && (nlen = strlen (name)) && nlen > len
	  && strcmp (name + nlen - len, dump_base_ext) == 0)
	{
	  char *p = xstrndup (name, nlen - len);
	  name = p;
	}

      aux_base_name = name;
    }
  else
    aux_base_name = "gccaux";

#ifndef HAVE_isl
  if (flag_graphite
      || flag_loop_nest_optimize
      || flag_graphite_identity
      || flag_loop_parallelize_all)
    sorry ("Graphite loop optimizations cannot be used (isl is not available) "
	   "(%<-fgraphite%>, %<-fgraphite-identity%>, "
	   "%<-floop-nest-optimize%>, %<-floop-parallelize-all%>)");
#endif

  if (flag_cf_protection != CF_NONE
      && !(flag_cf_protection & CF_SET))
    {
      if (flag_cf_protection == CF_FULL)
	{
	  error_at (UNKNOWN_LOCATION,
		    "%<-fcf-protection=full%> is not supported for this "
		    "target");
	  flag_cf_protection = CF_NONE;
	}
      if (flag_cf_protection == CF_BRANCH)
	{
	  error_at (UNKNOWN_LOCATION,
		    "%<-fcf-protection=branch%> is not supported for this "
		    "target");
	  flag_cf_protection = CF_NONE;
	}
      if (flag_cf_protection == CF_RETURN)
	{
	  error_at (UNKNOWN_LOCATION,
		    "%<-fcf-protection=return%> is not supported for this "
		    "target");
	  flag_cf_protection = CF_NONE;
	}
    }

  /* One region RA really helps to decrease the code size.  */
  if (!OPTION_SET_P (flag_ira_region))
    flag_ira_region
      = optimize_size || !optimize ? IRA_REGION_ONE : IRA_REGION_MIXED;

  if (!abi_version_at_least (2))
    {
      /* -fabi-version=1 support was removed after GCC 4.9.  */
      error_at (UNKNOWN_LOCATION,
		"%<-fabi-version=1%> is no longer supported");
      flag_abi_version = 2;
    }

  if (flag_non_call_exceptions)
    flag_asynchronous_unwind_tables = 1;
  if (flag_asynchronous_unwind_tables)
    flag_unwind_tables = 1;

  if (flag_value_profile_transformations)
    flag_profile_values = 1;

  /* Warn about options that are not supported on this machine.  */
#ifndef INSN_SCHEDULING
  if (flag_schedule_insns || flag_schedule_insns_after_reload)
    warning_at (UNKNOWN_LOCATION, 0,
		"instruction scheduling not supported on this target machine");
#endif
  if (!DELAY_SLOTS && flag_delayed_branch)
    warning_at (UNKNOWN_LOCATION, 0,
		"this target machine does not have delayed branches");

  user_label_prefix = USER_LABEL_PREFIX;
  if (flag_leading_underscore != -1)
    {
      /* If the default prefix is more complicated than "" or "_",
	 issue a warning and ignore this option.  */
      if (user_label_prefix[0] == 0 ||
	  (user_label_prefix[0] == '_' && user_label_prefix[1] == 0))
	{
	  user_label_prefix = flag_leading_underscore ? "_" : "";
	}
      else
	warning_at (UNKNOWN_LOCATION, 0,
		    "%<-f%sleading-underscore%> not supported on this "
		    "target machine", flag_leading_underscore ? "" : "no-");
    }

  /* If we are in verbose mode, write out the version and maybe all the
     option flags in use.  */
  if (version_flag)
    {
      /* We already printed the version header in main ().  */
      if (!quiet_flag)
	{
	  fputs ("options passed: ", stderr);
	  char *cmdline = gen_command_line_string (save_decoded_options,
						   save_decoded_options_count);

	  fputs (cmdline, stderr);
	  free (cmdline);
	  fputc ('\n', stderr);
	}
    }

  /* CTF is supported for only C at this time.  */
  if (!lang_GNU_C ()
      && ctf_debug_info_level > CTFINFO_LEVEL_NONE)
    {
      /* Compiling with -flto results in frontend language of GNU GIMPLE.  It
	 is not useful to warn in that case.  */
      if (!startswith (lang_hooks.name, "GNU GIMPLE"))
	inform (UNKNOWN_LOCATION,
		"CTF debug info requested, but not supported for %qs frontend",
		language_string);
      ctf_debug_info_level = CTFINFO_LEVEL_NONE;
    }

  if (flag_dump_final_insns && !flag_syntax_only && !no_backend)
    {
      FILE *final_output = fopen (flag_dump_final_insns, "w");
      if (!final_output)
	{
	  error_at (UNKNOWN_LOCATION,
		    "could not open final insn dump file %qs: %m",
		    flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
      else if (fclose (final_output))
	{
	  error_at (UNKNOWN_LOCATION,
		    "could not close zeroed insn dump file %qs: %m",
		    flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
    }

  /* A lot of code assumes write_symbols == NO_DEBUG if the debugging
     level is 0.  */
  if (debug_info_level == DINFO_LEVEL_NONE
      && ctf_debug_info_level == CTFINFO_LEVEL_NONE)
    write_symbols = NO_DEBUG;

  if (write_symbols == NO_DEBUG)
    ;
#ifdef DWARF2_DEBUGGING_INFO
  else if (dwarf_debuginfo_p ())
    debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef CTF_DEBUGGING_INFO
  else if (ctf_debuginfo_p ())
    debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef BTF_DEBUGGING_INFO
  else if (btf_debuginfo_p ())
    debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef VMS_DEBUGGING_INFO
  else if (write_symbols == VMS_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
    debug_hooks = &vmsdbg_debug_hooks;
#endif
#ifdef DWARF2_LINENO_DEBUGGING_INFO
  else if (write_symbols == DWARF2_DEBUG)
    debug_hooks = &dwarf2_lineno_debug_hooks;
#endif
  else
    {
      gcc_assert (debug_set_count (write_symbols) <= 1);
      error_at (UNKNOWN_LOCATION,
		"target system does not support the %qs debug format",
		debug_type_names[debug_set_to_format (write_symbols)]);
    }

  /* The debug hooks are used to implement -fdump-go-spec because it
     gives a simple and stable API for all the information we need to
     dump.  */
  if (flag_dump_go_spec != NULL)
    debug_hooks = dump_go_spec_init (flag_dump_go_spec, debug_hooks);

  if (!OPTION_SET_P (dwarf2out_as_loc_support))
    dwarf2out_as_loc_support = dwarf2out_default_as_loc_support ();
  if (!OPTION_SET_P (dwarf2out_as_locview_support))
    dwarf2out_as_locview_support = dwarf2out_default_as_locview_support ();

  if (!OPTION_SET_P (debug_variable_location_views))
    {
      debug_variable_location_views
	= (flag_var_tracking
	   && debug_info_level >= DINFO_LEVEL_NORMAL
	   && dwarf_debuginfo_p ()
	   && !dwarf_strict
	   && dwarf2out_as_loc_support
	   && dwarf2out_as_locview_support);
    }
  else if (debug_variable_location_views == -1 && dwarf_version != 5)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "without %<-gdwarf-5%>, "
		  "%<-gvariable-location-views=incompat5%> "
		  "is equivalent to %<-gvariable-location-views%>");
      debug_variable_location_views = 1;
    }

  if (debug_internal_reset_location_views == 2)
    {
      debug_internal_reset_location_views
	= (debug_variable_location_views
	   && targetm.reset_location_view);
    }
  else if (debug_internal_reset_location_views
	   && !debug_variable_location_views)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-ginternal-reset-location-views%> is forced disabled "
		  "without %<-gvariable-location-views%>");
      debug_internal_reset_location_views = 0;
    }

  if (!OPTION_SET_P (debug_inline_points))
    debug_inline_points = debug_variable_location_views;
  else if (debug_inline_points && !debug_nonbind_markers_p)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-ginline-points%> is forced disabled without "
		  "%<-gstatement-frontiers%>");
      debug_inline_points = 0;
    }

  if (!OPTION_SET_P (flag_tree_cselim))
    {
      if (HAVE_conditional_move)
	flag_tree_cselim = 1;
      else
	flag_tree_cselim = 0;
    }

  /* If auxiliary info generation is desired, open the output file.
     This goes in the same directory as the source file--unlike
     all the other output files.  */
  if (flag_gen_aux_info)
    {
      aux_info_file = fopen (aux_info_file_name, "w");
      if (aux_info_file == 0)
	fatal_error (UNKNOWN_LOCATION,
		     "cannot open %s: %m", aux_info_file_name);
    }

  if (!targetm_common.have_named_sections)
    {
      if (flag_function_sections)
	{
	  warning_at (UNKNOWN_LOCATION, 0,
		      "%<-ffunction-sections%> not supported for this target");
	  flag_function_sections = 0;
	}
      if (flag_data_sections)
	{
	  warning_at (UNKNOWN_LOCATION, 0,
		      "%<-fdata-sections%> not supported for this target");
	  flag_data_sections = 0;
	}
    }

  if (flag_prefetch_loop_arrays > 0 && !targetm.code_for_prefetch)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fprefetch-loop-arrays%> not supported for this target");
      flag_prefetch_loop_arrays = 0;
    }
  else if (flag_prefetch_loop_arrays > 0 && !targetm.have_prefetch ())
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fprefetch-loop-arrays%> not supported for this target "
		  "(try %<-march%> switches)");
      flag_prefetch_loop_arrays = 0;
    }

  /* This combination of options isn't handled for i386 targets and doesn't
     make much sense anyway, so don't allow it.  */
  if (flag_prefetch_loop_arrays > 0 && optimize_size)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fprefetch-loop-arrays%> is not supported with %<-Os%>");
      flag_prefetch_loop_arrays = 0;
    }

  /* The presence of IEEE signaling NaNs, implies all math can trap.  */
  if (flag_signaling_nans)
    flag_trapping_math = 1;

  /* We cannot reassociate if we want traps or signed zeros.  */
  if (flag_associative_math && (flag_trapping_math || flag_signed_zeros))
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fassociative-math%> disabled; other options take "
		  "precedence");
      flag_associative_math = 0;
    }

  if (flag_hardened && !HAVE_FHARDENED_SUPPORT)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fhardened%> not supported for this target");
      flag_hardened = 0;
    }

  /* -fstack-clash-protection is not currently supported on targets
     where the stack grows up.  */
  if (flag_stack_clash_protection && !STACK_GROWS_DOWNWARD)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fstack-clash-protection%> is not supported on targets "
		  "where the stack grows from lower to higher addresses");
      flag_stack_clash_protection = 0;
    }
  else if (flag_hardened)
    {
      if (!flag_stack_clash_protection
	   /* Don't enable -fstack-clash-protection when -fstack-check=
	      is used: it would result in confusing errors.  */
	   && flag_stack_check == NO_STACK_CHECK)
	flag_stack_clash_protection = 1;
      else if (flag_stack_check != NO_STACK_CHECK)
	warning_at (UNKNOWN_LOCATION, OPT_Whardened,
		    "%<-fstack-clash-protection%> is not enabled by "
		    "%<-fhardened%> because %<-fstack-check%> was "
		    "specified on the command line");
    }

  /* We cannot support -fstack-check= and -fstack-clash-protection at
     the same time.  */
  if (flag_stack_check != NO_STACK_CHECK && flag_stack_clash_protection)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fstack-check=%> and %<-fstack-clash-protection%> are "
		  "mutually exclusive; disabling %<-fstack-check=%>");
      flag_stack_check = NO_STACK_CHECK;
    }

  /* Targets must be able to place spill slots at lower addresses.  If the
     target already uses a soft frame pointer, the transition is trivial.  */
  if (!FRAME_GROWS_DOWNWARD && flag_stack_protect)
    {
      if (!flag_stack_protector_set_by_fhardened_p)
	warning_at (UNKNOWN_LOCATION, 0,
		    "%<-fstack-protector%> not supported for this target");
      flag_stack_protect = 0;
    }
  if (!flag_stack_protect)
    warn_stack_protect = 0;

  /* Address Sanitizer needs porting to each target architecture.  */

  if ((flag_sanitize & SANITIZE_ADDRESS)
      && !FRAME_GROWS_DOWNWARD)
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fsanitize=address%> and %<-fsanitize=kernel-address%> "
		  "are not supported for this target");
      flag_sanitize &= ~SANITIZE_ADDRESS;
    }

  if ((flag_sanitize & SANITIZE_USER_ADDRESS)
      && ((targetm.asan_shadow_offset == NULL)
	  || (targetm.asan_shadow_offset () == 0)))
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fsanitize=address%> not supported for this target");
      flag_sanitize &= ~SANITIZE_ADDRESS;
    }

  if ((flag_sanitize & SANITIZE_KERNEL_ADDRESS)
      && (targetm.asan_shadow_offset == NULL
	  && !asan_shadow_offset_set_p ()))
    {
      warning_at (UNKNOWN_LOCATION, 0,
		  "%<-fsanitize=kernel-address%> with stack protection "
		  "is not supported without %<-fasan-shadow-offset=%> "
		  "for this target");
      flag_sanitize &= ~SANITIZE_ADDRESS;
    }

  /* HWAsan requires top byte ignore feature in the backend.  */
  if (flag_sanitize & SANITIZE_HWADDRESS
      && ! targetm.memtag.can_tag_addresses ())
    {
      warning_at (UNKNOWN_LOCATION, 0, "%qs is not supported for this target",
		  "-fsanitize=hwaddress");
      flag_sanitize &= ~SANITIZE_HWADDRESS;
    }

  if (flag_sanitize & SANITIZE_SHADOW_CALL_STACK)
    {
      if (!targetm.have_shadow_call_stack)
	sorry ("%<-fsanitize=shadow-call-stack%> not supported "
	       "in current platform");
      else if (flag_exceptions)
	error_at (UNKNOWN_LOCATION, "%<-fsanitize=shadow-call-stack%> "
		  "requires %<-fno-exceptions%>");
    }

  HOST_WIDE_INT patch_area_size, patch_area_start;
  parse_and_check_patch_area (flag_patchable_function_entry, false,
			      &patch_area_size, &patch_area_start);

 /* Do not use IPA optimizations for register allocation if profiler is active
    or patchable function entries are inserted for run-time instrumentation
    or port does not emit prologue and epilogue as RTL.  */
  if (profile_flag || patch_area_size
      || !targetm.have_prologue () || !targetm.have_epilogue ())
    flag_ipa_ra = 0;

  /* Enable -Werror=coverage-mismatch when -Werror and -Wno-error
     have not been set.  */
  if (!OPTION_SET_P (warnings_are_errors))
    {
      if (warn_coverage_mismatch
	  && option_unspecified_p (OPT_Wcoverage_mismatch))
	diagnostic_classify_diagnostic (global_dc, OPT_Wcoverage_mismatch,
					DK_ERROR, UNKNOWN_LOCATION);
      if (warn_coverage_invalid_linenum
	  && option_unspecified_p (OPT_Wcoverage_invalid_line_number))
	diagnostic_classify_diagnostic (global_dc, OPT_Wcoverage_invalid_line_number,
					DK_ERROR, UNKNOWN_LOCATION);
    }

  /* Save the current optimization options.  */
  optimization_default_node
    = build_optimization_node (&global_options, &global_options_set);
  optimization_current_node = optimization_default_node;

  if (flag_checking >= 2)
    hash_table_sanitize_eq_limit
      = param_hash_table_verification_limit;

  if (flag_large_source_files)
    line_table->default_range_bits = 0;

  diagnose_options (&global_options, &global_options_set, UNKNOWN_LOCATION);

  /* Please don't change global_options after this point, those changes won't
     be reflected in optimization_{default,current}_node.  */
}

/* This function can be called multiple times to reinitialize the compiler
   back end when register classes or instruction sets have changed,
   before each function.  */
static void
backend_init_target (void)
{
  /* This depends on stack_pointer_rtx.  */
  init_fake_stack_mems ();

  /* Sets static_base_value[HARD_FRAME_POINTER_REGNUM], which is
     mode-dependent.  */
  init_alias_target ();

  /* Depends on HARD_FRAME_POINTER_REGNUM.  */
  if (!ira_use_lra_p)
    init_reload ();

  /* Depends on the enabled attribute.  */
  recog_init ();

  /* The following initialization functions need to generate rtl, so
     provide a dummy function context for them.  */
  init_dummy_function_start ();

  /* rtx_cost is mode-dependent, so cached values need to be recomputed
     on a mode change.  */
  init_expmed ();
  init_lower_subreg ();
  init_set_costs ();

  init_expr_target ();
  ira_init ();

  /* We may need to recompute regno_save_code[] and regno_restore_code[]
     after a mode change as well.  */
  caller_save_initialized_p = false;

  expand_dummy_function_end ();
}

/* Initialize the compiler back end.  This function is called only once,
   when starting the compiler.  */
static void
backend_init (void)
{
  init_emit_once ();

  init_rtlanal ();
  init_inline_once ();
  init_varasm_once ();
  save_register_info ();

  /* Middle end needs this initialization for default mem attributes
     used by early calls to make_decl_rtl.  */
  init_emit_regs ();

  /* Middle end needs this initialization for mode tables used to assign
     modes to vector variables.  */
  init_regs ();
}

/* Initialize things that are both lang-dependent and target-dependent.
   This function can be called more than once if target parameters change.  */
static void
lang_dependent_init_target (void)
{
  /* This creates various _DECL nodes, so needs to be called after the
     front end is initialized.  It also depends on the HAVE_xxx macros
     generated from the target machine description.  */
  init_optabs ();

  gcc_assert (!this_target_rtl->target_specific_initialized);
}

/* Perform initializations that are lang-dependent or target-dependent.
   but matters only for late optimizations and RTL generation.  */

static int rtl_initialized;

void
initialize_rtl (void)
{
  auto_timevar tv (g_timer, TV_INITIALIZE_RTL);

  /* Initialization done just once per compilation, but delayed
     till code generation.  */
  if (!rtl_initialized)
    ira_init_once ();
  rtl_initialized = true;

  /* Target specific RTL backend initialization.  */
  if (!this_target_rtl->target_specific_initialized)
    {
      backend_init_target ();
      this_target_rtl->target_specific_initialized = true;
    }
}

/* Language-dependent initialization.  Returns nonzero on success.  */
static int
lang_dependent_init (const char *name)
{
  location_t save_loc = input_location;
  if (!dump_base_name)
    {
      dump_base_name = name && name[0] ? name : "gccdump";

      /* We do not want to derive a non-empty dumpbase-ext from an
	 explicit -dumpbase argument, only from a defaulted
	 dumpbase.  */
      if (!dump_base_ext)
	{
	  const char *base = lbasename (dump_base_name);
	  const char *ext = strrchr (base, '.');
	  if (ext)
	    dump_base_ext = ext;
	}
    }

  /* Other front-end initialization.  */
  input_location = BUILTINS_LOCATION;
  if (lang_hooks.init () == 0)
    return 0;
  input_location = save_loc;

  if (!flag_wpa)
    {
      init_asm_output (name);

      if (!flag_generate_lto && !flag_compare_debug)
	{
	  /* If stack usage information is desired, open the output file.  */
	  if (flag_stack_usage)
	    stack_usage_file = open_auxiliary_file ("su");

	  /* If call graph information is desired, open the output file.  */
	  if (flag_callgraph_info)
	    {
	      callgraph_info_file = open_auxiliary_file ("ci");
	      /* Write the file header.  */
	      fprintf (callgraph_info_file,
		       "graph: { title: \"%s\"\n", main_input_filename);
	      bitmap_obstack_initialize (NULL);
	      callgraph_info_external_printed = BITMAP_ALLOC (NULL);
	    }
	}
      else
	flag_stack_usage = flag_callgraph_info = false;
    }

  /* This creates various _DECL nodes, so needs to be called after the
     front end is initialized.  */
  init_eh ();

  /* Do the target-specific parts of the initialization.  */
  lang_dependent_init_target ();

  if (!flag_wpa)
    {
      /* If dbx symbol table desired, initialize writing it and output the
	 predefined types.  */
      timevar_push (TV_SYMOUT);

      /* Now we have the correct original filename, we can initialize
	 debug output.  */
      (*debug_hooks->init) (name);

      timevar_pop (TV_SYMOUT);
    }

  return 1;
}


/* Reinitialize everything when target parameters, such as register usage,
   have changed.  */
void
target_reinit (void)
{
  if (no_backend)
    return;

  struct rtl_data saved_x_rtl;
  rtx *saved_regno_reg_rtx;
  tree saved_optimization_current_node;
  struct target_optabs *saved_this_fn_optabs;

  /* Temporarily switch to the default optimization node, so that
     *this_target_optabs is set to the default, not reflecting
     whatever a previous function used for the optimize
     attribute.  */
  saved_optimization_current_node = optimization_current_node;
  saved_this_fn_optabs = this_fn_optabs;
  if (saved_optimization_current_node != optimization_default_node)
    {
      optimization_current_node = optimization_default_node;
      cl_optimization_restore
	(&global_options, &global_options_set,
	 TREE_OPTIMIZATION (optimization_default_node));
    }
  this_fn_optabs = this_target_optabs;

  /* Save *crtl and regno_reg_rtx around the reinitialization
     to allow target_reinit being called even after prepare_function_start.  */
  saved_regno_reg_rtx = regno_reg_rtx;
  if (saved_regno_reg_rtx)
    {  
      saved_x_rtl = *crtl;
      memset (crtl, '\0', sizeof (*crtl));
      regno_reg_rtx = NULL;
    }

  this_target_rtl->target_specific_initialized = false;

  /* This initializes hard_frame_pointer, and calls init_reg_modes_target()
     to initialize reg_raw_mode[].  */
  init_emit_regs ();

  /* This invokes target hooks to set fixed_reg[] etc, which is
     mode-dependent.  */
  init_regs ();

  /* Reinitialize lang-dependent parts.  */
  lang_dependent_init_target ();

  /* Restore the original optimization node.  */
  if (saved_optimization_current_node != optimization_default_node)
    {
      optimization_current_node = saved_optimization_current_node;
      cl_optimization_restore (&global_options, &global_options_set,
			       TREE_OPTIMIZATION (optimization_current_node));
    }
  this_fn_optabs = saved_this_fn_optabs;

  /* Restore regno_reg_rtx at the end, as free_after_compilation from
     expand_dummy_function_end clears it.  */
  if (saved_regno_reg_rtx)
    {
      *crtl = saved_x_rtl;
      regno_reg_rtx = saved_regno_reg_rtx;
      saved_regno_reg_rtx = NULL;
    }
}

void
dump_memory_report (const char *header)
{
  /* Print significant header.  */
  fputc ('\n', stderr);
  for (unsigned i = 0; i < 80; i++)
    fputc ('#', stderr);
  fprintf (stderr, "\n# %-77s#\n", header);
  for (unsigned i = 0; i < 80; i++)
    fputc ('#', stderr);
  fputs ("\n\n", stderr);

  dump_line_table_statistics ();
  ggc_print_statistics ();
  stringpool_statistics ();
  dump_tree_statistics ();
  dump_gimple_statistics ();
  dump_rtx_statistics ();
  dump_alloc_pool_statistics ();
  dump_bitmap_statistics ();
  dump_hash_table_loc_statistics ();
  dump_vec_loc_statistics ();
  dump_ggc_loc_statistics ();
  dump_alias_stats (stderr);
  dump_pta_stats (stderr);
}

/* Clean up: close opened files, etc.  */

static void
finalize ()
{
  /* Close the dump files.  */
  if (flag_gen_aux_info)
    {
      fclose (aux_info_file);
      aux_info_file = NULL;
      if (seen_error ())
	unlink (aux_info_file_name);
    }

  /* Close non-debugging input and output files.  Take special care to note
     whether fclose returns an error, since the pages might still be on the
     buffer chain while the file is open.  */

  if (asm_out_file)
    {
      if (ferror (asm_out_file) != 0)
	fatal_error (input_location, "error writing to %s: %m", asm_file_name);
      if (fclose (asm_out_file) != 0)
	fatal_error (input_location, "error closing %s: %m", asm_file_name);
      asm_out_file = NULL;
    }

  if (stack_usage_file)
    {
      fclose (stack_usage_file);
      stack_usage_file = NULL;
    }

  if (callgraph_info_file)
    {
      fputs ("}\n", callgraph_info_file);
      fclose (callgraph_info_file);
      callgraph_info_file = NULL;
      BITMAP_FREE (callgraph_info_external_printed);
      bitmap_obstack_release (NULL);
    }

  if (seen_error ())
    coverage_remove_note_file ();

  if (!no_backend)
    {
      statistics_fini ();
      debuginfo_fini ();

      g->get_passes ()->finish_optimization_passes ();

      lra_finish_once ();
    }

  if (mem_report)
    dump_memory_report ("Final");

  if (profile_report)
    dump_profile_report ();

  if (flag_dbg_cnt_list)
    dbg_cnt_list_all_counters ();

  /* Language-specific end of compilation actions.  */
  lang_hooks.finish ();
}

static bool
standard_type_bitsize (int bitsize)
{
  /* As a special exception, we always want __int128 enabled if possible.  */
  if (bitsize == 128)
    return false;
  if (bitsize == CHAR_TYPE_SIZE
      || bitsize == SHORT_TYPE_SIZE
      || bitsize == INT_TYPE_SIZE
      || bitsize == LONG_TYPE_SIZE
      || bitsize == LONG_LONG_TYPE_SIZE)
    return true;
  return false;
}

/* Initialize the compiler, and compile the input file.  */
static void
do_compile ()
{
  /* Don't do any more if an error has already occurred.  */
  if (!seen_error ())
    {
      int i;

      timevar_start (TV_PHASE_SETUP);

      if (flag_save_optimization_record)
	{
	  dump_context::get ().set_json_writer (new optrecord_json_writer ());
	}

      /* This must be run always, because it is needed to compute the FP
	 predefined macros, such as __LDBL_MAX__, for targets using non
	 default FP formats.  */
      init_adjust_machine_modes ();
      init_derived_machine_modes ();

      /* This must happen after the backend has a chance to process
	 command line options, but before the parsers are
	 initialized.  */
      for (i = 0; i < NUM_INT_N_ENTS; i ++)
	if (targetm.scalar_mode_supported_p (int_n_data[i].m)
	    && ! standard_type_bitsize (int_n_data[i].bitsize))
	  int_n_enabled_p[i] = true;
	else
	  int_n_enabled_p[i] = false;

      /* Initialize mpfrs exponent range.  This is important to get
         underflow/overflow in a reasonable timeframe.  */
      machine_mode mode;
      int min_exp = -1;
      int max_exp = 1;
      FOR_EACH_MODE_IN_CLASS (mode, MODE_FLOAT)
	if (SCALAR_FLOAT_MODE_P (mode))
	  {
	    const real_format *fmt = REAL_MODE_FORMAT (mode);
	    if (fmt)
	      {
		/* fmt->emin - fmt->p + 1 should be enough but the
		   back-and-forth dance in real_to_decimal_for_mode we
		   do for checking fails due to rounding effects then.  */
		if ((fmt->emin - fmt->p) < min_exp)
		  min_exp = fmt->emin - fmt->p;
		if (fmt->emax > max_exp)
		  max_exp = fmt->emax;
	      }
	  }
      /* E.g. mpc_norm assumes it can square a number without bothering with
	 with range scaling, so until that is fixed, double the minimum
	 and maximum exponents, plus add some buffer for arithmetics
	 on the squared numbers.  */
      if (mpfr_set_emin (2 * (min_exp - 1))
	  || mpfr_set_emax (2 * (max_exp + 1)))
	sorry ("mpfr not configured to handle all floating modes");

      /* Set up the back-end if requested.  */
      if (!no_backend)
	backend_init ();

      /* Language-dependent initialization.  Returns true on success.  */
      if (lang_dependent_init (main_input_filename))
        {
          /* Initialize yet another pass.  */

          ggc_protect_identifiers = true;

	  symtab->initialize ();
          init_final (main_input_filename);
          coverage_init (aux_base_name);
          statistics_init ();
          debuginfo_init ();
          invoke_plugin_callbacks (PLUGIN_START_UNIT, NULL);

          timevar_stop (TV_PHASE_SETUP);

          compile_file ();
        }
      else
        {
          timevar_stop (TV_PHASE_SETUP);
        }

      timevar_start (TV_PHASE_FINALIZE);

      finalize ();

      timevar_stop (TV_PHASE_FINALIZE);
    }
}

toplev::toplev (timer *external_timer,
		bool init_signals)
  : m_use_TV_TOTAL (external_timer == NULL),
    m_init_signals (init_signals)
{
  if (external_timer)
    g_timer = external_timer;
}

toplev::~toplev ()
{
  if (g_timer && m_use_TV_TOTAL)
    {
      g_timer->stop (TV_TOTAL);
      g_timer->print (stderr);
      delete g_timer;
      g_timer = NULL;
    }
}

/* Potentially call timevar_init (which will create g_timevars if it
   doesn't already exist).  */

void
toplev::start_timevars ()
{
  if (time_report || !quiet_flag  || flag_detailed_statistics)
    timevar_init ();

  timevar_start (TV_TOTAL);
}

/* Handle -fself-test.   */

void
toplev::run_self_tests ()
{
#if CHECKING_P
  /* Reset some state.  */
  input_location = UNKNOWN_LOCATION;
  bitmap_obstack_initialize (NULL);

  /* Run the tests; any failures will lead to an abort of the process.
     Use "make selftests-gdb" to run under the debugger.  */
  ::selftest::run_tests ();

  /* Cleanup.  */
  bitmap_obstack_release (NULL);
#else
  inform (UNKNOWN_LOCATION, "self-tests are not enabled in this build");
#endif /* #if CHECKING_P */
}

/* Entry point of cc1, cc1plus, jc1, f771, etc.
   Exit code is FATAL_EXIT_CODE if can't open files or if there were
   any errors, or SUCCESS_EXIT_CODE if compilation succeeded.

   It is not safe to call this function more than once.  */

int
toplev::main (int argc, char **argv)
{
  /* Parsing and gimplification sometimes need quite large stack.
     Increase stack size limits if possible.  */
  stack_limit_increase (64 * 1024 * 1024);

  expandargv (&argc, &argv);

  /* Initialization of GCC's environment, and diagnostics.  */
  general_init (argv[0], m_init_signals);

  /* One-off initialization of options that does not need to be
     repeated when options are added for particular functions.  */
  init_options_once ();
  init_opts_obstack ();

  /* Initialize global options structures; this must be repeated for
     each structure used for parsing options.  */
  init_options_struct (&global_options, &global_options_set);
  lang_hooks.init_options_struct (&global_options);

  /* Init GGC heuristics must be caller after we initialize
     options.  */
  init_ggc_heuristics ();

  /* Convert the options to an array.  */
  decode_cmdline_options_to_array_default_mask (argc,
						CONST_CAST2 (const char **,
							     char **, argv),
						&save_decoded_options,
						&save_decoded_options_count);

  /* Save Optimization decoded options.  */
  save_opt_decoded_options = new vec<cl_decoded_option> ();
  for (unsigned i = 1; i < save_decoded_options_count; ++i)
    if (save_decoded_options[i].opt_index < cl_options_count
	&& cl_options[save_decoded_options[i].opt_index].flags & CL_OPTIMIZATION)
      save_opt_decoded_options->safe_push (save_decoded_options[i]);

  /* Perform language-specific options initialization.  */
  lang_hooks.init_options (save_decoded_options_count, save_decoded_options);

  /* Parse the options and do minimal processing; basically just
     enough to default flags appropriately.  */
  decode_options (&global_options, &global_options_set,
		  save_decoded_options, save_decoded_options_count,
		  UNKNOWN_LOCATION, global_dc,
		  targetm.target_option.override);

  handle_common_deferred_options ();

  init_local_tick ();

  initialize_plugins ();

  /* Handle the dump options now that plugins have had a chance to install new
     passes.  */
  handle_deferred_dump_options ();

  if (version_flag)
    print_version (stderr, "", true);

  if (help_flag)
    print_plugins_help (stderr, "");

  /* Exit early if we can (e.g. -help).  */
  if (!exit_after_options)
    {
      /* Just in case lang_hooks.post_options ends up calling a debug_hook.
	 This can happen with incorrect pre-processed input. */
      debug_hooks = &do_nothing_debug_hooks;
      /* Allow the front end to perform consistency checks and do further
	 initialization based on the command line options.  This hook also
	 sets the original filename if appropriate (e.g. foo.i -> foo.c)
	 so we can correctly initialize debug output.  */
      no_backend = lang_hooks.post_options (&main_input_filename);

      process_options ();

      if (m_use_TV_TOTAL)
	start_timevars ();
      do_compile ();

      if (flag_self_test && !seen_error ())
	{
	  if (no_backend)
	    error_at (UNKNOWN_LOCATION, "self-tests incompatible with %<-E%>");
	  else
	    run_self_tests ();
	}
    }

  if (warningcount || errorcount || werrorcount)
    print_ignored_options ();

  /* Invoke registered plugin callbacks if any.  Some plugins could
     emit some diagnostics here.  */
  invoke_plugin_callbacks (PLUGIN_FINISH, NULL);

  if (auto edit_context_ptr = global_dc->get_edit_context ())
    {
      pretty_printer pp;
      pp_show_color (&pp) = pp_show_color (global_dc->printer);
      edit_context_ptr->print_diff (&pp, true);
      pp_flush (&pp);
    }

  diagnostic_finish (global_dc);

  finalize_plugins ();

  after_memory_report = true;

  if (seen_error () || werrorcount)
    return (FATAL_EXIT_CODE);

  return (SUCCESS_EXIT_CODE);
}

/* For those that want to, this function aims to clean up enough state that
   you can call toplev::main again. */
void
toplev::finalize (void)
{
  no_backend = false;
  rtl_initialized = false;
  this_target_rtl->target_specific_initialized = false;

  /* Needs to be called before cgraph_cc_finalize since it uses symtab.  */
  ipa_reference_cc_finalize ();
  ipa_fnsummary_cc_finalize ();
  ipa_modref_cc_finalize ();
  ipa_edge_modifications_finalize ();
  ipa_icf_cc_finalize ();

  ipa_prop_cc_finalize ();
  ipa_profile_cc_finalize ();
  ipa_sra_cc_finalize ();
  cgraph_cc_finalize ();
  cgraphunit_cc_finalize ();
  symtab_thunks_cc_finalize ();
  dwarf2cfi_cc_finalize ();
  dwarf2out_cc_finalize ();
  gcse_cc_finalize ();
  ipa_cp_cc_finalize ();
  ira_costs_cc_finalize ();
  tree_cc_finalize ();
  reginfo_cc_finalize ();

  /* save_decoded_options uses opts_obstack, so these must
     be cleaned up together.  */
  obstack_free (&opts_obstack, NULL);
  XDELETEVEC (save_decoded_options);
  save_decoded_options = NULL;
  save_decoded_options_count = 0;

  ggc_common_finalize ();

  /* Clean up the context (and pass_manager etc). */
  delete g;
  g = NULL;

}
