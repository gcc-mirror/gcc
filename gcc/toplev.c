/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011 Free Software Foundation, Inc.

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
#include "tm.h"
#include "line-map.h"
#include "input.h"
#include "tree.h"
#include "realmpfr.h"	/* For GMP/MPFR/MPC versions, in print_version.  */
#include "version.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "insn-attr.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "output.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "expr.h"
#include "basic-block.h"
#include "intl.h"
#include "ggc.h"
#include "graph.h"
#include "regs.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"
#include "tree-pretty-print.h"
#include "params.h"
#include "reload.h"
#include "ira.h"
#include "dwarf2asm.h"
#include "debug.h"
#include "target.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "cfgloop.h" /* for init_set_costs */
#include "hosthooks.h"
#include "cgraph.h"
#include "opts.h"
#include "opts-diagnostic.h"
#include "coverage.h"
#include "value-prof.h"
#include "alloc-pool.h"
#include "tree-mudflap.h"
#include "gimple.h"
#include "tree-ssa-alias.h"
#include "plugin.h"

#if defined(DBX_DEBUGGING_INFO) || defined(XCOFF_DEBUGGING_INFO)
#include "dbxout.h"
#endif

#ifdef SDB_DEBUGGING_INFO
#include "sdbout.h"
#endif

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data
				   declarations for e.g. AIX 4.x.  */
#endif

static void general_init (const char *);
static void do_compile (void);
static void process_options (void);
static void backend_init (void);
static int lang_dependent_init (const char *);
static void init_asm_output (const char *);
static void finalize (bool);

static void crash_signal (int) ATTRIBUTE_NORETURN;
static void compile_file (void);

/* True if we don't need a backend (e.g. preprocessing only).  */
static bool no_backend;

/* Length of line when printing switch values.  */
#define MAX_LINE 75

/* Decoded options, and number of such options.  */
struct cl_decoded_option *save_decoded_options;
unsigned int save_decoded_options_count;

/* Used to enable -fvar-tracking, -fweb and -frename-registers according
   to optimize in process_options ().  */
#define AUTODETECT_VALUE 2

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

/* True if the user has tagged the function with the 'section'
   attribute.  */

bool user_defined_section_attribute = false;

struct target_flag_state default_target_flag_state;
#if SWITCHABLE_TARGET
struct target_flag_state *this_target_flag_state = &default_target_flag_state;
#else
#define this_target_flag_state (&default_target_flag_state)
#endif

/* The user symbol prefix after having resolved same.  */
const char *user_label_prefix;

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *aux_info_file;
FILE *stack_usage_file = NULL;
FILE *dump_file = NULL;
const char *dump_file_name;

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

/* Initialize local_tick with a random number or -1 if
   flag_random_seed is set.  */

static void
init_local_tick (void)
{
  if (!flag_random_seed)
    {
      /* Try urandom first. Time of day is too likely to collide. 
	 In case of any error we just use the local tick. */

      int fd = open ("/dev/urandom", O_RDONLY);
      if (fd >= 0)
        {
          read (fd, &random_seed, sizeof (random_seed));
          close (fd);
        }

      /* Now get the tick anyways  */
#ifdef HAVE_GETTIMEOFDAY
      {
	struct timeval tv;

	gettimeofday (&tv, NULL);
	local_tick = tv.tv_sec * 1000 + tv.tv_usec / 1000;
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

/* Set up a default flag_random_seed and local_tick, unless the user
   already specified one.  Must be called after init_local_tick.  */

static void
init_random_seed (void)
{
  if (flag_random_seed)
    {
      char *endp;

      /* When the driver passed in a hex number don't crc it again */
      random_seed = strtoul (flag_random_seed, &endp, 0);
      if (!(endp > flag_random_seed && *endp == 0))
        random_seed = crc32_string (0, flag_random_seed);
    }
  else if (!random_seed)
    random_seed = local_tick ^ getpid ();  /* Old racey fallback method */
}

/* Obtain the random_seed.  Unless NOINIT, initialize it if
   it's not provided in the command line.  */

HOST_WIDE_INT
get_random_seed (bool noinit)
{
  if (!flag_random_seed && !noinit)
    init_random_seed ();
  return random_seed;
}

/* Modify the random_seed string to VAL.  Return its previous
   value.  */

const char *
set_random_seed (const char *val)
{
  const char *old = flag_random_seed;
  flag_random_seed = val;
  return old;
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
  if (CODE_CONTAINS_STRUCT (TREE_CODE (decl), TS_DECL_WITH_VIS)
      && DECL_DEFER_OUTPUT (decl) != 0)
    DECL_DEFER_OUTPUT (decl) = 0;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SIZE (decl) == 0)
    lang_hooks.finish_incomplete_decl (decl);
}

/* A subroutine of wrapup_global_declarations.  Decide whether or not DECL
   needs to be output.  Return true if it is output.  */

bool
wrapup_global_declaration_2 (tree decl)
{
  if (TREE_ASM_WRITTEN (decl) || DECL_EXTERNAL (decl)
      || (TREE_CODE (decl) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (decl)))
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

  if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl))
    {
      struct varpool_node *node;
      bool needed = true;
      node = varpool_get_node (decl);

      if (!node && flag_ltrans)
	needed = false;
      else if (node && node->finalized)
	needed = false;
      else if (node && node->alias)
	needed = false;
      else if (!cgraph_global_info_ready
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

/* A subroutine of check_global_declarations.  Issue appropriate warnings
   for the global declaration DECL.  */

void
check_global_declaration_1 (tree decl)
{
  /* Warn about any function declared static but not defined.  We don't
     warn about variables, because many programs have static variables
     that exist only to get some text into the object file.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_INITIAL (decl) == 0
      && DECL_EXTERNAL (decl)
      && ! DECL_ARTIFICIAL (decl)
      && ! TREE_NO_WARNING (decl)
      && ! TREE_PUBLIC (decl)
      && (warn_unused_function
	  || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
    {
      if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	pedwarn (input_location, 0, "%q+F used but never defined", decl);
      else
	warning (OPT_Wunused_function, "%q+F declared %<static%> but never defined", decl);
      /* This symbol is effectively an "extern" declaration now.  */
      TREE_PUBLIC (decl) = 1;
    }

  /* Warn about static fns or vars defined but not used.  */
  if (((warn_unused_function && TREE_CODE (decl) == FUNCTION_DECL)
       /* We don't warn about "static const" variables because the
	  "rcs_id" idiom uses that construction.  */
       || (warn_unused_variable
	   && TREE_CODE (decl) == VAR_DECL && ! TREE_READONLY (decl)))
      && ! DECL_IN_SYSTEM_HEADER (decl)
      && ! TREE_USED (decl)
      /* The TREE_USED bit for file-scope decls is kept in the identifier,
	 to handle multiple external decls in different scopes.  */
      && ! (DECL_NAME (decl) && TREE_USED (DECL_NAME (decl)))
      && ! DECL_EXTERNAL (decl)
      && ! TREE_PUBLIC (decl)
      /* A volatile variable might be used in some non-obvious way.  */
      && ! TREE_THIS_VOLATILE (decl)
      /* Global register variables must be declared to reserve them.  */
      && ! (TREE_CODE (decl) == VAR_DECL && DECL_REGISTER (decl))
      /* Otherwise, ask the language.  */
      && lang_hooks.decls.warn_unused_global (decl))
    warning ((TREE_CODE (decl) == FUNCTION_DECL)
	     ? OPT_Wunused_function
             : OPT_Wunused_variable,
	     "%q+D defined but not used", decl);
}

/* Issue appropriate warnings for the global declarations in VEC (of
   which there are LEN).  */

void
check_global_declarations (tree *vec, int len)
{
  int i;

  for (i = 0; i < len; i++)
    check_global_declaration_1 (vec[i]);
}

/* Emit debugging information for all global declarations in VEC.  */

void
emit_debug_global_declarations (tree *vec, int len)
{
  int i;

  /* Avoid confusing the debug information machinery when there are errors.  */
  if (seen_error ())
    return;

  timevar_push (TV_SYMOUT);
  for (i = 0; i < len; i++)
    debug_hooks->global_decl (vec[i]);
  timevar_pop (TV_SYMOUT);
}

/* Compile an entire translation unit.  Write a file of assembly
   output and various debugging dumps.  */

static void
compile_file (void)
{
  timevar_start (TV_PHASE_PARSING);
  timevar_push (TV_PARSE_GLOBAL);

  /* Call the parser, which parses the entire file (calling
     rest_of_compilation for each function).  */
  lang_hooks.parse_file ();

  timevar_pop (TV_PARSE_GLOBAL);
  timevar_stop (TV_PHASE_PARSING);

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */

  if (flag_syntax_only || flag_wpa)
    return;

  ggc_protect_identifiers = false;

  /* This must also call finalize_compilation_unit.  */
  lang_hooks.decls.final_write_globals ();

  if (seen_error ())
    return;

  timevar_start (TV_PHASE_LATE_ASM);

  /* Compilation unit is finalized.  When producing non-fat LTO object, we are
     basically finished.  */
  if (in_lto_p || !flag_lto || flag_fat_lto_objects)
    {
      /* Likewise for mudflap static object registrations.  */
      if (flag_mudflap)
	mudflap_finish_file ();

      output_shared_constant_pool ();
      output_object_blocks ();
      finish_tm_clone_pairs ();

      /* Write out any pending weak symbol declarations.  */
      weak_finish ();

      /* This must be at the end before unwind and debug info.
	 Some target ports emit PIC setup thunks here.  */
      targetm.asm_out.code_end ();

      /* Do dbx symbols.  */
      timevar_push (TV_SYMOUT);

    #if defined DWARF2_DEBUGGING_INFO || defined DWARF2_UNWIND_INFO
      if (dwarf2out_do_frame ())
	dwarf2out_frame_finish ();
    #endif

      (*debug_hooks->finish) (main_input_filename);
      timevar_pop (TV_SYMOUT);

      /* Output some stuff at end of file if nec.  */

      dw2_output_indirect_constants ();

      /* Flush any pending external directives.  */
      process_pending_assemble_externals ();
   }

  /* Emit LTO marker if LTO info has been previously emitted.  This is
     used by collect2 to determine whether an object file contains IL.
     We used to emit an undefined reference here, but this produces
     link errors if an object file with IL is stored into a shared
     library without invoking lto1.  */
  if (flag_generate_lto)
    {
#if defined ASM_OUTPUT_ALIGNED_DECL_COMMON
      ASM_OUTPUT_ALIGNED_DECL_COMMON (asm_out_file, NULL_TREE,
				      "__gnu_lto_v1",
				      (unsigned HOST_WIDE_INT) 1, 8);
#elif defined ASM_OUTPUT_ALIGNED_COMMON
      ASM_OUTPUT_ALIGNED_COMMON (asm_out_file, "__gnu_lto_v1",
				 (unsigned HOST_WIDE_INT) 1, 8);
#else
      ASM_OUTPUT_COMMON (asm_out_file, "__gnu_lto_v1",
			 (unsigned HOST_WIDE_INT) 1,
			 (unsigned HOST_WIDE_INT) 1);
#endif
      /* Let linker plugin know that this is a slim object and must be LTOed
         even when user did not ask for it.  */
      if (!flag_fat_lto_objects)
        {
#if defined ASM_OUTPUT_ALIGNED_DECL_COMMON
	  ASM_OUTPUT_ALIGNED_DECL_COMMON (asm_out_file, NULL_TREE,
					  "__gnu_lto_slim",
					  (unsigned HOST_WIDE_INT) 1, 8);
#elif defined ASM_OUTPUT_ALIGNED_COMMON
	  ASM_OUTPUT_ALIGNED_COMMON (asm_out_file, "__gnu_lto_slim",
				     (unsigned HOST_WIDE_INT) 1, 8);
#else
	  ASM_OUTPUT_COMMON (asm_out_file, "__gnu_lto_slim",
			     (unsigned HOST_WIDE_INT) 1,
			     (unsigned HOST_WIDE_INT) 1);
#endif
        }
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

  /* Invoke registered plugin callbacks.  */
  invoke_plugin_callbacks (PLUGIN_FINISH_UNIT, NULL);

  /* This must be at the end.  Some target ports emit end of file directives
     into the assembly file here, and hence we can not output anything to the
     assembly file after this point.  */
  targetm.asm_out.file_end ();

  timevar_stop (TV_PHASE_LATE_ASM);
}

/* Print version information to FILE.
   Each line begins with INDENT (for the case where FILE is the
   assembler output file).  */

void
print_version (FILE *file, const char *indent)
{
  static const char fmt1[] =
#ifdef __GNUC__
    N_("%s%s%s %sversion %s (%s)\n%s\tcompiled by GNU C version %s, ")
#else
    N_("%s%s%s %sversion %s (%s) compiled by CC, ")
#endif
    ;
  static const char fmt2[] =
    N_("GMP version %s, MPFR version %s, MPC version %s\n");
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
#define GCC_GMP_STRINGIFY_VERSION2(X) GCC_GMP_STRINGIFY_VERSION3(X)
#define GCC_GMP_VERSION_NUM(X,Y,Z) (((X) << 16L) | ((Y) << 8) | (Z))
#define GCC_GMP_VERSION \
  GCC_GMP_VERSION_NUM(__GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, __GNU_MP_VERSION_PATCHLEVEL)
#if GCC_GMP_VERSION < GCC_GMP_VERSION_NUM(4,3,0) && __GNU_MP_VERSION_PATCHLEVEL == 0
#define GCC_GMP_STRINGIFY_VERSION GCC_GMP_STRINGIFY_VERSION2(__GNU_MP_VERSION) "." \
  GCC_GMP_STRINGIFY_VERSION2(__GNU_MP_VERSION_MINOR)
#else
#define GCC_GMP_STRINGIFY_VERSION GCC_GMP_STRINGIFY_VERSION2(__GNU_MP_VERSION) "." \
  GCC_GMP_STRINGIFY_VERSION2(__GNU_MP_VERSION_MINOR) "." \
  GCC_GMP_STRINGIFY_VERSION2(__GNU_MP_VERSION_PATCHLEVEL)
#endif
  fprintf (file,
	   file == stderr ? _(fmt2) : fmt2,
	   GCC_GMP_STRINGIFY_VERSION, MPFR_VERSION_STRING, MPC_VERSION_STRING);
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
  fprintf (file,
	   file == stderr ? _(fmt4) : fmt4,
	   indent, *indent != 0 ? " " : "",
	   PARAM_VALUE (GGC_MIN_EXPAND), PARAM_VALUE (GGC_MIN_HEAPSIZE));

  print_plugins_versions (file, indent);
}

static int
print_to_asm_out_file (print_switch_type type, const char * text)
{
  bool prepend_sep = true;

  switch (type)
    {
    case SWITCH_TYPE_LINE_END:
      putc ('\n', asm_out_file);
      return 1;

    case SWITCH_TYPE_LINE_START:
      fputs (ASM_COMMENT_START, asm_out_file);
      return strlen (ASM_COMMENT_START);

    case SWITCH_TYPE_DESCRIPTIVE:
      if (ASM_COMMENT_START[0] == 0)
	prepend_sep = false;
      /* Drop through.  */
    case SWITCH_TYPE_PASSED:
    case SWITCH_TYPE_ENABLED:
      if (prepend_sep)
	fputc (' ', asm_out_file);
      fputs (text, asm_out_file);
      /* No need to return the length here as
	 print_single_switch has already done it.  */
      return 0;

    default:
      return -1;
    }
}

static int
print_to_stderr (print_switch_type type, const char * text)
{
  switch (type)
    {
    case SWITCH_TYPE_LINE_END:
      putc ('\n', stderr);
      return 1;

    case SWITCH_TYPE_LINE_START:
      return 0;

    case SWITCH_TYPE_PASSED:
    case SWITCH_TYPE_ENABLED:
      fputc (' ', stderr);
      /* Drop through.  */

    case SWITCH_TYPE_DESCRIPTIVE:
      fputs (text, stderr);
      /* No need to return the length here as
	 print_single_switch has already done it.  */
      return 0;

    default:
      return -1;
    }
}

/* Print an option value and return the adjusted position in the line.
   ??? print_fn doesn't handle errors, eg disk full; presumably other
   code will catch a disk full though.  */

static int
print_single_switch (print_switch_fn_type print_fn,
		     int pos,
		     print_switch_type type,
		     const char * text)
{
  /* The ultrix fprintf returns 0 on success, so compute the result
     we want here since we need it for the following test.  The +1
     is for the separator character that will probably be emitted.  */
  int len = strlen (text) + 1;

  if (pos != 0
      && pos + len > MAX_LINE)
    {
      print_fn (SWITCH_TYPE_LINE_END, NULL);
      pos = 0;
    }

  if (pos == 0)
    pos += print_fn (SWITCH_TYPE_LINE_START, NULL);

  print_fn (type, text);
  return pos + len;
}

/* Print active target switches using PRINT_FN.
   POS is the current cursor position and MAX is the size of a "line".
   Each line begins with INDENT and ends with TERM.
   Each switch is separated from the next by SEP.  */

static void
print_switch_values (print_switch_fn_type print_fn)
{
  int pos = 0;
  size_t j;

  /* Fill in the -frandom-seed option, if the user didn't pass it, so
     that it can be printed below.  This helps reproducibility.  */
  if (!flag_random_seed)
    init_random_seed ();

  /* Print the options as passed.  */
  pos = print_single_switch (print_fn, pos,
			     SWITCH_TYPE_DESCRIPTIVE, _("options passed: "));

  for (j = 1; j < save_decoded_options_count; j++)
    {
      switch (save_decoded_options[j].opt_index)
	{
	case OPT_o:
	case OPT_d:
	case OPT_dumpbase:
	case OPT_dumpdir:
	case OPT_auxbase:
	case OPT_quiet:
	case OPT_version:
	  /* Ignore these.  */
	  continue;
	}

      pos = print_single_switch (print_fn, pos, SWITCH_TYPE_PASSED,
				 save_decoded_options[j].orig_option_with_args_text);
    }

  if (pos > 0)
    print_fn (SWITCH_TYPE_LINE_END, NULL);

  /* Print the -f and -m options that have been enabled.
     We don't handle language specific options but printing argv
     should suffice.  */
  pos = print_single_switch (print_fn, 0,
			     SWITCH_TYPE_DESCRIPTIVE, _("options enabled: "));

  for (j = 0; j < cl_options_count; j++)
    if (cl_options[j].cl_report
	&& option_enabled (j, &global_options) > 0)
      pos = print_single_switch (print_fn, pos,
				 SWITCH_TYPE_ENABLED, cl_options[j].opt_text);

  print_fn (SWITCH_TYPE_LINE_END, NULL);
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
      else
	asm_out_file = fopen (asm_file_name, "w");
      if (asm_out_file == 0)
	fatal_error ("can%'t open %s for writing: %m", asm_file_name);
    }

  if (!flag_syntax_only)
    {
      targetm.asm_out.file_start ();

      if (flag_record_gcc_switches)
	{
	  if (targetm.asm_out.record_gcc_switches)
	    {
	      /* Let the target know that we are about to start recording.  */
	      targetm.asm_out.record_gcc_switches (SWITCH_TYPE_DESCRIPTIVE,
						   NULL);
	      /* Now record the switches.  */
	      print_switch_values (targetm.asm_out.record_gcc_switches);
	      /* Let the target know that the recording is over.  */
	      targetm.asm_out.record_gcc_switches (SWITCH_TYPE_DESCRIPTIVE,
						   NULL);
	    }
	  else
	    inform (input_location, "-frecord-gcc-switches is not supported by the current target");
	}

      if (flag_verbose_asm)
	{
	  /* Print the list of switches in effect
	     into the assembler file as comments.  */
	  print_version (asm_out_file, ASM_COMMENT_START);
	  print_switch_values (print_to_asm_out_file);
	  putc ('\n', asm_out_file);
	}
    }
}

/* A helper function; used as the reallocator function for cpp's line
   table.  */
static void *
realloc_for_line_map (void *ptr, size_t len)
{
  return GGC_RESIZEVAR (void, ptr, len);
}

/* A helper function: used as the allocator function for
   identifier_to_locale.  */
static void *
alloc_for_identifier_to_locale (size_t len)
{
  return ggc_alloc_atomic (len);
}

/* Output stack usage information.  */
void
output_stack_usage (void)
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
  if (current_function_pushed_stack_size > 0)
    {
      stack_usage += current_function_pushed_stack_size;
      stack_usage_kind = DYNAMIC_BOUNDED;
    }

  /* Now on to the tricky part: dynamic stack allocation.  */
  if (current_function_allocates_dynamic_stack_space)
    {
      if (current_function_has_unbounded_dynamic_stack_size)
	stack_usage_kind = DYNAMIC;
      else
	stack_usage_kind = DYNAMIC_BOUNDED;

      /* Add the size even in the unbounded case, this can't hurt.  */
      stack_usage += current_function_dynamic_stack_size;
    }

  if (flag_stack_usage)
    {
      expanded_location loc
	= expand_location (DECL_SOURCE_LOCATION (current_function_decl));
      const char *raw_id, *id;

      /* Strip the scope prefix if any.  */
      raw_id = lang_hooks.decl_printable_name (current_function_decl, 2);
      id = strrchr (raw_id, '.');
      if (id)
	id++;
      else
	id = raw_id;

      fprintf (stack_usage_file,
	       "%s:%d:%d:%s\t"HOST_WIDE_INT_PRINT_DEC"\t%s\n",
	       lbasename (loc.file),
	       loc.line,
	       loc.column,
	       id,
	       stack_usage,
	       stack_usage_kind_str[stack_usage_kind]);
    }

  if (warn_stack_usage >= 0)
    {
      if (stack_usage_kind == DYNAMIC)
	warning (OPT_Wstack_usage_, "stack usage might be unbounded");
      else if (stack_usage > warn_stack_usage)
	{
	  if (stack_usage_kind == DYNAMIC_BOUNDED)
	    warning (OPT_Wstack_usage_, "stack usage might be %wd bytes",
		     stack_usage);
	  else
	    warning (OPT_Wstack_usage_, "stack usage is %wd bytes",
		     stack_usage);
	}
    }
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
    fatal_error ("can%'t open %s for writing: %m", filename);
  free (filename);
  return file;
}

/* Initialization of the front end environment, before command line
   options are parsed.  Signal handlers, internationalization etc.
   ARGV0 is main's argv[0].  */
static void
general_init (const char *argv0)
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
  /* FIXME: This should probably be moved to C-family
     language-specific initializations.  */
  /* By default print macro expansion contexts in the diagnostic
     finalizer -- for tokens resulting from macro expansion.  */
  diagnostic_finalizer (global_dc) = virt_loc_aware_diagnostic_finalizer;

  global_dc->show_caret
    = global_options_init.x_flag_diagnostics_show_caret;
  global_dc->show_option_requested
    = global_options_init.x_flag_diagnostics_show_option;
  global_dc->show_column
    = global_options_init.x_flag_show_column;
  global_dc->internal_error = plugins_internal_error_function;
  global_dc->option_enabled = option_enabled;
  global_dc->option_state = &global_options;
  global_dc->option_name = option_name;

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

  /* Initialize the garbage-collector, string pools and tree type hash
     table.  */
  init_ggc ();
  init_stringpool ();
  line_table = ggc_alloc_line_maps ();
  linemap_init (line_table);
  line_table->reallocator = realloc_for_line_map;
  line_table->round_alloc_size = ggc_round_alloc_size;
  init_ttree ();

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  /* Register the language-independent parameters.  */
  global_init_params ();

  /* This must be done after global_init_params but before argument
     processing.  */
  init_ggc_heuristics();
  init_optimization_passes ();
  statistics_early_init ();
  finish_params ();
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

/* Default the align_* variables to 1 if they're still unset, and
   set up the align_*_log variables.  */
static void
init_alignments (void)
{
  if (align_loops <= 0)
    align_loops = 1;
  if (align_loops_max_skip > align_loops)
    align_loops_max_skip = align_loops - 1;
  align_loops_log = floor_log2 (align_loops * 2 - 1);
  if (align_jumps <= 0)
    align_jumps = 1;
  if (align_jumps_max_skip > align_jumps)
    align_jumps_max_skip = align_jumps - 1;
  align_jumps_log = floor_log2 (align_jumps * 2 - 1);
  if (align_labels <= 0)
    align_labels = 1;
  align_labels_log = floor_log2 (align_labels * 2 - 1);
  if (align_labels_max_skip > align_labels)
    align_labels_max_skip = align_labels - 1;
  if (align_functions <= 0)
    align_functions = 1;
  align_functions_log = floor_log2 (align_functions * 2 - 1);
}

/* Process the options that have been parsed.  */
static void
process_options (void)
{
  /* Just in case lang_hooks.post_options ends up calling a debug_hook.
     This can happen with incorrect pre-processed input. */
  debug_hooks = &do_nothing_debug_hooks;

  maximum_field_alignment = initial_max_fld_align * BITS_PER_UNIT;

  /* Allow the front end to perform consistency checks and do further
     initialization based on the command line options.  This hook also
     sets the original filename if appropriate (e.g. foo.i -> foo.c)
     so we can correctly initialize debug output.  */
  no_backend = lang_hooks.post_options (&main_input_filename);

  /* Some machines may reject certain combinations of options.  */
  targetm.target_option.override ();

  /* Avoid any informative notes in the second run of -fcompare-debug.  */
  if (flag_compare_debug) 
    diagnostic_inhibit_notes (global_dc);

  if (flag_section_anchors && !target_supports_section_anchors_p ())
    {
      warning (OPT_fsection_anchors,
	       "this target does not support %qs", "-fsection-anchors");
      flag_section_anchors = 0;
    }

  if (flag_short_enums == 2)
    flag_short_enums = targetm.default_short_enums ();

  /* Set aux_base_name if not already set.  */
  if (aux_base_name)
    ;
  else if (main_input_filename)
    {
      char *name = xstrdup (lbasename (main_input_filename));

      strip_off_ending (name, strlen (name));
      aux_base_name = name;
    }
  else
    aux_base_name = "gccaux";

#ifndef HAVE_cloog
  if (flag_graphite
      || flag_graphite_identity
      || flag_loop_block
      || flag_loop_interchange
      || flag_loop_strip_mine
      || flag_loop_parallelize_all)
    sorry ("Graphite loop optimizations cannot be used (-fgraphite, "
	   "-fgraphite-identity, -floop-block, "
	   "-floop-interchange, -floop-strip-mine, -floop-parallelize-all, "
	   "and -ftree-loop-linear)");
#endif

  if (flag_mudflap && flag_lto)
    sorry ("mudflap cannot be used together with link-time optimization");

  /* One region RA really helps to decrease the code size.  */
  if (flag_ira_region == IRA_REGION_AUTODETECT)
    flag_ira_region
      = optimize_size || !optimize ? IRA_REGION_ONE : IRA_REGION_MIXED;

  if (flag_strict_volatile_bitfields > 0 && !abi_version_at_least (2))
    {
      warning (0, "-fstrict-volatile-bitfields disabled; "
	       "it is incompatible with ABI versions < 2");
      flag_strict_volatile_bitfields = 0;
    }

  /* Unrolling all loops implies that standard loop unrolling must also
     be done.  */
  if (flag_unroll_all_loops)
    flag_unroll_loops = 1;

  /* web and rename-registers help when run after loop unrolling.  */
  if (flag_web == AUTODETECT_VALUE)
    flag_web = flag_unroll_loops || flag_peel_loops;

  if (flag_rename_registers == AUTODETECT_VALUE)
    flag_rename_registers = flag_unroll_loops || flag_peel_loops;

  if (flag_non_call_exceptions)
    flag_asynchronous_unwind_tables = 1;
  if (flag_asynchronous_unwind_tables)
    flag_unwind_tables = 1;

  if (flag_value_profile_transformations)
    flag_profile_values = 1;

  /* Warn about options that are not supported on this machine.  */
#ifndef INSN_SCHEDULING
  if (flag_schedule_insns || flag_schedule_insns_after_reload)
    warning (0, "instruction scheduling not supported on this target machine");
#endif
#ifndef DELAY_SLOTS
  if (flag_delayed_branch)
    warning (0, "this target machine does not have delayed branches");
#endif

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
	warning (0, "-f%sleading-underscore not supported on this target machine",
		 flag_leading_underscore ? "" : "no-");
    }

  /* If we are in verbose mode, write out the version and maybe all the
     option flags in use.  */
  if (version_flag)
    {
      print_version (stderr, "");
      if (! quiet_flag)
	print_switch_values (print_to_stderr);
    }

  if (flag_syntax_only)
    {
      write_symbols = NO_DEBUG;
      profile_flag = 0;
    }

  if (flag_gtoggle)
    {
      if (debug_info_level == DINFO_LEVEL_NONE)
	{
	  debug_info_level = DINFO_LEVEL_NORMAL;

	  if (write_symbols == NO_DEBUG)
	    write_symbols = PREFERRED_DEBUGGING_TYPE;
	}
      else
	debug_info_level = DINFO_LEVEL_NONE;
    }

  if (flag_dump_final_insns && !flag_syntax_only && !no_backend)
    {
      FILE *final_output = fopen (flag_dump_final_insns, "w");
      if (!final_output)
	{
	  error ("could not open final insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
      else if (fclose (final_output))
	{
	  error ("could not close zeroed insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
    }

  /* A lot of code assumes write_symbols == NO_DEBUG if the debugging
     level is 0.  */
  if (debug_info_level == DINFO_LEVEL_NONE)
    write_symbols = NO_DEBUG;

  if (write_symbols == NO_DEBUG)
    ;
#if defined(DBX_DEBUGGING_INFO)
  else if (write_symbols == DBX_DEBUG)
    debug_hooks = &dbx_debug_hooks;
#endif
#if defined(XCOFF_DEBUGGING_INFO)
  else if (write_symbols == XCOFF_DEBUG)
    debug_hooks = &xcoff_debug_hooks;
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (write_symbols == SDB_DEBUG)
    debug_hooks = &sdb_debug_hooks;
#endif
#ifdef DWARF2_DEBUGGING_INFO
  else if (write_symbols == DWARF2_DEBUG)
    debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef VMS_DEBUGGING_INFO
  else if (write_symbols == VMS_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
    debug_hooks = &vmsdbg_debug_hooks;
#endif
  else
    error ("target system does not support the \"%s\" debug format",
	   debug_type_names[write_symbols]);

  /* We know which debug output will be used so we can set flag_var_tracking
     and flag_var_tracking_uninit if the user has not specified them.  */
  if (debug_info_level < DINFO_LEVEL_NORMAL
      || debug_hooks->var_location == do_nothing_debug_hooks.var_location)
    {
      if (flag_var_tracking == 1
	  || flag_var_tracking_uninit == 1)
        {
	  if (debug_info_level < DINFO_LEVEL_NORMAL)
	    warning (0, "variable tracking requested, but useless unless "
		     "producing debug info");
	  else
	    warning (0, "variable tracking requested, but not supported "
		     "by this debug format");
	}
      flag_var_tracking = 0;
      flag_var_tracking_uninit = 0;
    }

  /* The debug hooks are used to implement -fdump-go-spec because it
     gives a simple and stable API for all the information we need to
     dump.  */
  if (flag_dump_go_spec != NULL)
    debug_hooks = dump_go_spec_init (flag_dump_go_spec, debug_hooks);

  /* If the user specifically requested variable tracking with tagging
     uninitialized variables, we need to turn on variable tracking.
     (We already determined above that variable tracking is feasible.)  */
  if (flag_var_tracking_uninit)
    flag_var_tracking = 1;

  if (flag_var_tracking == AUTODETECT_VALUE)
    flag_var_tracking = optimize >= 1;

  if (flag_var_tracking_assignments == AUTODETECT_VALUE)
    flag_var_tracking_assignments = flag_var_tracking
      && !(flag_selective_scheduling || flag_selective_scheduling2);

  if (flag_var_tracking_assignments_toggle)
    flag_var_tracking_assignments = !flag_var_tracking_assignments;

  if (flag_var_tracking_assignments && !flag_var_tracking)
    flag_var_tracking = flag_var_tracking_assignments = -1;

  if (flag_var_tracking_assignments
      && (flag_selective_scheduling || flag_selective_scheduling2))
    warning (0, "var-tracking-assignments changes selective scheduling");

  if (flag_tree_cselim == AUTODETECT_VALUE)
#ifdef HAVE_conditional_move
    flag_tree_cselim = 1;
#else
    flag_tree_cselim = 0;
#endif

  /* If auxiliary info generation is desired, open the output file.
     This goes in the same directory as the source file--unlike
     all the other output files.  */
  if (flag_gen_aux_info)
    {
      aux_info_file = fopen (aux_info_file_name, "w");
      if (aux_info_file == 0)
	fatal_error ("can%'t open %s: %m", aux_info_file_name);
    }

  if (!targetm_common.have_named_sections)
    {
      if (flag_function_sections)
	{
	  warning (0, "-ffunction-sections not supported for this target");
	  flag_function_sections = 0;
	}
      if (flag_data_sections)
	{
	  warning (0, "-fdata-sections not supported for this target");
	  flag_data_sections = 0;
	}
    }

  if (flag_function_sections && profile_flag)
    {
      warning (0, "-ffunction-sections disabled; it makes profiling impossible");
      flag_function_sections = 0;
    }

#ifndef HAVE_prefetch
  if (flag_prefetch_loop_arrays > 0)
    {
      warning (0, "-fprefetch-loop-arrays not supported for this target");
      flag_prefetch_loop_arrays = 0;
    }
#else
  if (flag_prefetch_loop_arrays > 0 && !HAVE_prefetch)
    {
      warning (0, "-fprefetch-loop-arrays not supported for this target (try -march switches)");
      flag_prefetch_loop_arrays = 0;
    }
#endif

  /* This combination of options isn't handled for i386 targets and doesn't
     make much sense anyway, so don't allow it.  */
  if (flag_prefetch_loop_arrays > 0 && optimize_size)
    {
      warning (0, "-fprefetch-loop-arrays is not supported with -Os");
      flag_prefetch_loop_arrays = 0;
    }

  /* The presence of IEEE signaling NaNs, implies all math can trap.  */
  if (flag_signaling_nans)
    flag_trapping_math = 1;

  /* We cannot reassociate if we want traps or signed zeros.  */
  if (flag_associative_math && (flag_trapping_math || flag_signed_zeros))
    {
      warning (0, "-fassociative-math disabled; other options take precedence");
      flag_associative_math = 0;
    }

  /* With -fcx-limited-range, we do cheap and quick complex arithmetic.  */
  if (flag_cx_limited_range)
    flag_complex_method = 0;

  /* With -fcx-fortran-rules, we do something in-between cheap and C99.  */
  if (flag_cx_fortran_rules)
    flag_complex_method = 1;

  /* Targets must be able to place spill slots at lower addresses.  If the
     target already uses a soft frame pointer, the transition is trivial.  */
  if (!FRAME_GROWS_DOWNWARD && flag_stack_protect)
    {
      warning (0, "-fstack-protector not supported for this target");
      flag_stack_protect = 0;
    }
  if (!flag_stack_protect)
    warn_stack_protect = 0;

  /* ??? Unwind info is not correct around the CFG unless either a frame
     pointer is present or A_O_A is set.  Fixing this requires rewriting
     unwind info generation to be aware of the CFG and propagating states
     around edges.  */
  if (flag_unwind_tables && !ACCUMULATE_OUTGOING_ARGS
      && flag_omit_frame_pointer)
    {
      warning (0, "unwind tables currently require a frame pointer "
	       "for correctness");
      flag_omit_frame_pointer = 0;
    }

  /* Enable -Werror=coverage-mismatch when -Werror and -Wno-error
     have not been set.  */
  if (!global_options_set.x_warnings_are_errors
      && warn_coverage_mismatch
      && (global_dc->classify_diagnostic[OPT_Wcoverage_mismatch] ==
          DK_UNSPECIFIED))
    diagnostic_classify_diagnostic (global_dc, OPT_Wcoverage_mismatch,
                                    DK_ERROR, UNKNOWN_LOCATION);

  /* Save the current optimization options.  */
  optimization_default_node = build_optimization_node ();
  optimization_current_node = optimization_default_node;
}

/* This function can be called multiple times to reinitialize the compiler
   back end when register classes or instruction sets have changed,
   before each function.  */
static void
backend_init_target (void)
{
  /* Initialize alignment variables.  */
  init_alignments ();

  /* This reinitializes hard_frame_pointer, and calls init_reg_modes_target()
     to initialize reg_raw_mode[].  */
  init_emit_regs ();

  /* This invokes target hooks to set fixed_reg[] etc, which is
     mode-dependent.  */
  init_regs ();

  /* This depends on stack_pointer_rtx.  */
  init_fake_stack_mems ();

  /* Sets static_base_value[HARD_FRAME_POINTER_REGNUM], which is
     mode-dependent.  */
  init_alias_target ();

  /* Depends on HARD_FRAME_POINTER_REGNUM.  */
  init_reload ();

  /* The following initialization functions need to generate rtl, so
     provide a dummy function context for them.  */
  init_dummy_function_start ();

  /* rtx_cost is mode-dependent, so cached values need to be recomputed
     on a mode change.  */
  init_expmed ();
  init_lower_subreg ();

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

  /* Initialize the target-specific back end pieces.  */
  ira_init_once ();
  backend_init_target ();
}

/* Initialize excess precision settings.  */
static void
init_excess_precision (void)
{
  /* Adjust excess precision handling based on the target options.  If
     the front end cannot handle it, flag_excess_precision_cmdline
     will already have been set accordingly in the post_options
     hook.  */
  gcc_assert (flag_excess_precision_cmdline != EXCESS_PRECISION_DEFAULT);
  flag_excess_precision = flag_excess_precision_cmdline;
  if (flag_unsafe_math_optimizations)
    flag_excess_precision = EXCESS_PRECISION_FAST;
  if (flag_excess_precision == EXCESS_PRECISION_STANDARD)
    {
      int flt_eval_method = TARGET_FLT_EVAL_METHOD;
      switch (flt_eval_method)
	{
	case -1:
	case 0:
	  /* Either the target acts unpredictably (-1) or has all the
	     operations required not to have excess precision (0).  */
	  flag_excess_precision = EXCESS_PRECISION_FAST;
	  break;
	case 1:
	case 2:
	  /* In these cases, predictable excess precision makes
	     sense.  */
	  break;
	default:
	  /* Any other implementation-defined FLT_EVAL_METHOD values
	     require the compiler to handle the associated excess
	     precision rules in excess_precision_type.  */
	  gcc_unreachable ();
	}
    }
}

/* Initialize things that are both lang-dependent and target-dependent.
   This function can be called more than once if target parameters change.  */
static void
lang_dependent_init_target (void)
{
  /* This determines excess precision settings.  */
  init_excess_precision ();

  /* This creates various _DECL nodes, so needs to be called after the
     front end is initialized.  It also depends on the HAVE_xxx macros
     generated from the target machine description.  */
  init_optabs ();

  /* The following initialization functions need to generate rtl, so
     provide a dummy function context for them.  */
  init_dummy_function_start ();

  /* Do the target-specific parts of expr initialization.  */
  init_expr_target ();

  /* Although the actions of these functions are language-independent,
     they use optabs, so we cannot call them from backend_init.  */
  init_set_costs ();
  ira_init ();

  expand_dummy_function_end ();
}

/* Language-dependent initialization.  Returns nonzero on success.  */
static int
lang_dependent_init (const char *name)
{
  location_t save_loc = input_location;
  if (dump_base_name == 0)
    dump_base_name = name && name[0] ? name : "gccdump";

  /* Other front-end initialization.  */
  input_location = BUILTINS_LOCATION;
  if (lang_hooks.init () == 0)
    return 0;
  input_location = save_loc;

  if (!flag_wpa)
    {
      init_asm_output (name);

      /* If stack usage information is desired, open the output file.  */
      if (flag_stack_usage)
	stack_usage_file = open_auxiliary_file ("su");
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
  struct rtl_data saved_x_rtl;
  rtx *saved_regno_reg_rtx;

  /* Save *crtl and regno_reg_rtx around the reinitialization
     to allow target_reinit being called even after prepare_function_start.  */
  saved_regno_reg_rtx = regno_reg_rtx;
  if (saved_regno_reg_rtx)
    {  
      saved_x_rtl = *crtl;
      memset (crtl, '\0', sizeof (*crtl));
      regno_reg_rtx = NULL;
    }

  /* Reinitialize RTL backend.  */
  backend_init_target ();

  /* Reinitialize lang-dependent parts.  */
  lang_dependent_init_target ();

  /* And restore it at the end, as free_after_compilation from
     expand_dummy_function_end clears it.  */
  if (saved_regno_reg_rtx)
    {
      *crtl = saved_x_rtl;
      regno_reg_rtx = saved_regno_reg_rtx;
      saved_regno_reg_rtx = NULL;
    }
}

void
dump_memory_report (bool final)
{
  dump_line_table_statistics ();
  ggc_print_statistics ();
  stringpool_statistics ();
  dump_tree_statistics ();
  dump_gimple_statistics ();
  dump_rtx_statistics ();
  dump_alloc_pool_statistics ();
  dump_bitmap_statistics ();
  dump_vec_loc_statistics ();
  dump_ggc_loc_statistics (final);
  dump_alias_stats (stderr);
  dump_pta_stats (stderr);
}

/* Clean up: close opened files, etc.  */

static void
finalize (bool no_backend)
{
  /* Close the dump files.  */
  if (flag_gen_aux_info)
    {
      fclose (aux_info_file);
      if (seen_error ())
	unlink (aux_info_file_name);
    }

  /* Close non-debugging input and output files.  Take special care to note
     whether fclose returns an error, since the pages might still be on the
     buffer chain while the file is open.  */

  if (asm_out_file)
    {
      if (ferror (asm_out_file) != 0)
	fatal_error ("error writing to %s: %m", asm_file_name);
      if (fclose (asm_out_file) != 0)
	fatal_error ("error closing %s: %m", asm_file_name);
    }

  if (stack_usage_file)
    fclose (stack_usage_file);

  if (!no_backend)
    {
      statistics_fini ();

      finish_optimization_passes ();

      ira_finish_once ();
    }

  if (mem_report)
    dump_memory_report (true);

  /* Language-specific end of compilation actions.  */
  lang_hooks.finish ();
}

/* Initialize the compiler, and compile the input file.  */
static void
do_compile (void)
{
  /* Initialize timing first.  The C front ends read the main file in
     the post_options hook, and C++ does file timings.  */
  if (time_report || !quiet_flag  || flag_detailed_statistics)
    timevar_init ();
  timevar_start (TV_TOTAL);

  process_options ();

  /* Don't do any more if an error has already occurred.  */
  if (!seen_error ())
    {
      timevar_start (TV_PHASE_SETUP);

      /* This must be run always, because it is needed to compute the FP
	 predefined macros, such as __LDBL_MAX__, for targets using non
	 default FP formats.  */
      init_adjust_machine_modes ();

      /* Set up the back-end if requested.  */
      if (!no_backend)
	backend_init ();

      /* Language-dependent initialization.  Returns true on success.  */
      if (lang_dependent_init (main_input_filename))
        {
          /* Initialize yet another pass.  */

          ggc_protect_identifiers = true;

          init_cgraph ();
          init_final (main_input_filename);
          coverage_init (aux_base_name);
          statistics_init ();
          invoke_plugin_callbacks (PLUGIN_START_UNIT, NULL);

          timevar_stop (TV_PHASE_SETUP);

          compile_file ();
        }
      else
        {
          timevar_stop (TV_PHASE_SETUP);
        }

      timevar_start (TV_PHASE_FINALIZE);

      finalize (no_backend);

      timevar_stop (TV_PHASE_FINALIZE);
    }

  /* Stop timing and print the times.  */
  timevar_stop (TV_TOTAL);
  timevar_print (stderr);
}

/* Entry point of cc1, cc1plus, jc1, f771, etc.
   Exit code is FATAL_EXIT_CODE if can't open files or if there were
   any errors, or SUCCESS_EXIT_CODE if compilation succeeded.

   It is not safe to call this function more than once.  */

int
toplev_main (int argc, char **argv)
{
  /* Parsing and gimplification sometimes need quite large stack.
     Increase stack size limits if possible.  */
  stack_limit_increase (64 * 1024 * 1024);

  expandargv (&argc, &argv);

  /* Initialization of GCC's environment, and diagnostics.  */
  general_init (argv[0]);

  /* One-off initialization of options that does not need to be
     repeated when options are added for particular functions.  */
  init_options_once ();

  /* Initialize global options structures; this must be repeated for
     each structure used for parsing options.  */
  init_options_struct (&global_options, &global_options_set);
  lang_hooks.init_options_struct (&global_options);

  /* Convert the options to an array.  */
  decode_cmdline_options_to_array_default_mask (argc,
						CONST_CAST2 (const char **,
							     char **, argv),
						&save_decoded_options,
						&save_decoded_options_count);

  /* Perform language-specific options initialization.  */
  lang_hooks.init_options (save_decoded_options_count, save_decoded_options);

  /* Parse the options and do minimal processing; basically just
     enough to default flags appropriately.  */
  decode_options (&global_options, &global_options_set,
		  save_decoded_options, save_decoded_options_count,
		  UNKNOWN_LOCATION, global_dc);

  handle_common_deferred_options ();

  init_local_tick ();

  initialize_plugins ();

  if (version_flag)
    print_version (stderr, "");

  if (help_flag)
    print_plugins_help (stderr, "");

  /* Exit early if we can (e.g. -help).  */
  if (!exit_after_options)
    do_compile ();

  if (warningcount || errorcount)
    print_ignored_options ();
  diagnostic_finish (global_dc);

  /* Invoke registered plugin callbacks if any.  */
  invoke_plugin_callbacks (PLUGIN_FINISH, NULL);

  finalize_plugins ();
  if (seen_error ())
    return (FATAL_EXIT_CODE);

  return (SUCCESS_EXIT_CODE);
}
