/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include <signal.h>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#include "line-map.h"
#include "input.h"
#include "tree.h"
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
#include "params.h"
#include "reload.h"
#include "ira.h"
#include "dwarf2asm.h"
#include "integrate.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "cfgloop.h"
#include "hosthooks.h"
#include "cgraph.h"
#include "opts.h"
#include "coverage.h"
#include "value-prof.h"
#include "alloc-pool.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "gimple.h"
#include "tree-ssa-alias.h"
#include "plugin.h"

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
#include "dwarf2out.h"
#endif

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
static void finalize (void);

static void crash_signal (int) ATTRIBUTE_NORETURN;
static void setup_core_dumping (void);
static void compile_file (void);

/* Nonzero to dump debug info whilst parsing (-dy option).  */
static int set_yydebug;

/* True if we don't need a backend (e.g. preprocessing only).  */
static bool no_backend;

/* Length of line when printing switch values.  */
#define MAX_LINE 75

/* Name of program invoked, sans directories.  */

const char *progname;

/* Copy of argument vector to toplev_main.  */
static const char **save_argv;

/* Name of top-level original source file (what was input to cpp).
   This comes from the #-command at the beginning of the actual input.
   If there isn't any there, then this is the cc1 input file name.  */

const char *main_input_filename;

/* Used to enable -fvar-tracking, -fweb and -frename-registers according
   to optimize and default_debug_hooks in process_options ().  */
#define AUTODETECT_VALUE 2

/* Current position in real source file.  */

location_t input_location;

struct line_maps *line_table;

/* Name to use as base of names for dump output files.  */

const char *dump_base_name;

/* Directory used for dump output files.  */

const char *dump_dir_name;

/* Name to use as a base for auxiliary output files.  */

const char *aux_base_name;

/* Prefix for profile data files */
const char *profile_data_prefix;

/* A mask of target_flags that includes bit X if X was set or cleared
   on the command line.  */

int target_flags_explicit;

/* Debug hooks - dependent upon command line options.  */

const struct gcc_debug_hooks *debug_hooks;

/* Debug hooks - target default.  */

static const struct gcc_debug_hooks *default_debug_hooks;

/* Other flags saying which kinds of debugging dump have been requested.  */

int rtl_dump_and_exit;
int flag_print_asm_name;
enum graph_dump_types graph_dump_format;

/* Name for output file of assembly code, specified with -o.  */

const char *asm_file_name;

/* Nonzero means do optimizations.  -O.
   Particular numeric values stand for particular amounts of optimization;
   thus, -O2 stores 2 here.  However, the optimizations beyond the basic
   ones are not controlled directly by this variable.  Instead, they are
   controlled by individual `flag_...' variables that are defaulted
   based on this variable.  */

int optimize = 0;

/* Nonzero means optimize for size.  -Os.
   The only valid values are zero and nonzero. When optimize_size is
   nonzero, optimize defaults to 2, but certain individual code
   bloating optimizations are disabled.  */

int optimize_size = 0;

/* True if this is the lto front end.  This is used to disable
   gimple generation and lowering passes that are normally run on the
   output of a front end.  These passes must be bypassed for lto since
   they have already been done before the gimple was written.  */

bool in_lto_p = false;

/* Nonzero if we should write GIMPLE bytecode for link-time optimization.  */

int flag_generate_lto;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to the FUNC_BEGIN label of the current function, or NULL
   if none.  */
const char * current_function_func_begin_label;

/* Nonzero means to collect statistics which might be expensive
   and to print them when we are done.  */
int flag_detailed_statistics = 0;

/* A random sequence of characters, unless overridden by user.  */
static const char *flag_random_seed;

/* A local time stamp derived from the time of compilation. It will be
   zero if the system cannot provide a time.  It will be -1u, if the
   user has specified a particular random seed.  */
unsigned local_tick;

/* -f flags.  */

/* Nonzero means `char' should be signed.  */

int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  A value
   of 2 means it has not yet been initialized.  */

int flag_short_enums;

/* Nonzero if structures and unions should be returned in memory.

   This should only be defined if compatibility with another compiler or
   with an ABI is needed, because it results in slower code.  */

#ifndef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1
#endif

/* Nonzero for -fpcc-struct-return: return values the same way PCC does.  */

int flag_pcc_struct_return = DEFAULT_PCC_STRUCT_RETURN;

/* 0 means straightforward implementation of complex divide acceptable.
   1 means wide ranges of inputs must work for complex divide.
   2 means C99-like requirements for complex multiply and divide.  */

int flag_complex_method = 1;

/* Nonzero means we should be saving declaration info into a .X file.  */

int flag_gen_aux_info = 0;

/* Specified name of aux-info file.  */

const char *aux_info_file_name;

/* Nonzero if we are compiling code for a shared library, zero for
   executable.  */

int flag_shlib;

/* Generate code for GNU or NeXT Objective-C runtime environment.  */

#ifdef NEXT_OBJC_RUNTIME
int flag_next_runtime = 1;
#else
int flag_next_runtime = 0;
#endif

/* Set to the default thread-local storage (tls) model to use.  */

enum tls_model flag_tls_default = TLS_MODEL_GLOBAL_DYNAMIC;

/* Set the default region and algorithm for the integrated register
   allocator.  */

enum ira_algorithm flag_ira_algorithm = IRA_ALGORITHM_CB;
enum ira_region flag_ira_region = IRA_REGION_MIXED;

/* Set the default value for -fira-verbose.  */

unsigned int flag_ira_verbose = 5;

/* Set the default for excess precision.  */

enum excess_precision flag_excess_precision_cmdline = EXCESS_PRECISION_DEFAULT;
enum excess_precision flag_excess_precision = EXCESS_PRECISION_DEFAULT;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */

int flag_pedantic_errors = 0;

/* Nonzero means make permerror produce warnings instead of errors.  */

int flag_permissive = 0;

/* -dA causes debug commentary information to be produced in
   the generated assembly code (to make it more readable).  This option
   is generally only of use to those who actually need to read the
   generated assembly code (perhaps while debugging the compiler itself).
   Currently, this switch is only used by dwarfout.c; however, it is intended
   to be a catchall for printing debug information in the assembler file.  */

int flag_debug_asm = 0;

/* -dP causes the rtl to be emitted as a comment in assembly.  */

int flag_dump_rtl_in_asm = 0;

/* When non-NULL, indicates that whenever space is allocated on the
   stack, the resulting stack pointer must not pass this
   address---that is, for stacks that grow downward, the stack pointer
   must always be greater than or equal to this address; for stacks
   that grow upward, the stack pointer must be less than this address.
   At present, the rtx may be either a REG or a SYMBOL_REF, although
   the support provided depends on the backend.  */
rtx stack_limit_rtx;

/* Positive if we should track variables, negative if we should run
   the var-tracking pass only to discard debug annotations, zero if
   we're not to run it.  When flag_var_tracking == AUTODETECT_VALUE it
   will be set according to optimize, debug_info_level and debug_hooks
   in process_options ().  */
int flag_var_tracking = AUTODETECT_VALUE;

/* Positive if we should track variables at assignments, negative if
   we should run the var-tracking pass only to discard debug
   annotations.  When flag_var_tracking_assignments ==
   AUTODETECT_VALUE it will be set according to flag_var_tracking.  */
int flag_var_tracking_assignments = AUTODETECT_VALUE;

/* Nonzero if we should toggle flag_var_tracking_assignments after
   processing options and computing its default.  */
int flag_var_tracking_assignments_toggle = 0;

/* Type of stack check.  */
enum stack_check_type flag_stack_check = NO_STACK_CHECK;

/* True if the user has tagged the function with the 'section'
   attribute.  */

bool user_defined_section_attribute = false;

/* Values of the -falign-* flags: how much to align labels in code.
   0 means `use default', 1 means `don't align'.
   For each variable, there is an _log variant which is the power
   of two not less than the variable, for .align output.  */

int align_loops_log;
int align_loops_max_skip;
int align_jumps_log;
int align_jumps_max_skip;
int align_labels_log;
int align_labels_max_skip;
int align_functions_log;

typedef struct
{
  const char *const string;
  int *const variable;
  const int on_value;
}
lang_independent_options;

/* Nonzero if subexpressions must be evaluated from left-to-right.  */
int flag_evaluation_order = 0;

/* The user symbol prefix after having resolved same.  */
const char *user_label_prefix;

static const param_info lang_independent_params[] = {
#define DEFPARAM(ENUM, OPTION, HELP, DEFAULT, MIN, MAX) \
  { OPTION, DEFAULT, false, MIN, MAX, HELP },
#include "params.def"
#undef DEFPARAM
  { NULL, 0, false, 0, 0, NULL }
};

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *aux_info_file;
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

/* Initialize local_tick with the time of day, or -1 if
   flag_random_seed is set.  */

static void
init_local_tick (void)
{
  if (!flag_random_seed)
    {
      /* Get some more or less random data.  */
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
  unsigned HOST_WIDE_INT value;
  static char random_seed[HOST_BITS_PER_WIDE_INT / 4 + 3];

  value = local_tick ^ getpid ();

  sprintf (random_seed, HOST_WIDE_INT_PRINT_HEX, value);
  flag_random_seed = random_seed;
}

/* Obtain the random_seed string.  Unless NOINIT, initialize it if
   it's not provided in the command line.  */

const char *
get_random_seed (bool noinit)
{
  if (!flag_random_seed && !noinit)
    init_random_seed ();
  return flag_random_seed;
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

/* Decode the string P as an integral parameter.
   If the string is indeed an integer return its numeric value else
   issue an Invalid Option error for the option PNAME and return DEFVAL.
   If PNAME is zero just return DEFVAL, do not call error.  */

int
read_integral_parameter (const char *p, const char *pname, const int  defval)
{
  const char *endp = p;

  while (*endp)
    {
      if (ISDIGIT (*endp))
	endp++;
      else
	break;
    }

  if (*endp != 0)
    {
      if (pname != 0)
	error ("invalid option argument %qs", pname);
      return defval;
    }

  return atoi (p);
}

#if GCC_VERSION < 3004

/* The functions floor_log2 and exact_log2 are defined as inline
   functions in toplev.h if GCC_VERSION >= 3004.  The definitions here
   are used for older versions of gcc.  */

/* Given X, an unsigned number, return the largest int Y such that 2**Y <= X.
   If X is 0, return -1.  */

int
floor_log2 (unsigned HOST_WIDE_INT x)
{
  int t = 0;

  if (x == 0)
    return -1;

  if (HOST_BITS_PER_WIDE_INT > 64)
    if (x >= (unsigned HOST_WIDE_INT) 1 << (t + 64))
      t += 64;
  if (HOST_BITS_PER_WIDE_INT > 32)
    if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 32))
      t += 32;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 16))
    t += 16;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 8))
    t += 8;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 4))
    t += 4;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 2))
    t += 2;
  if (x >= ((unsigned HOST_WIDE_INT) 1) << (t + 1))
    t += 1;

  return t;
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.  */

int
exact_log2 (unsigned HOST_WIDE_INT x)
{
  if (x != (x & -x))
    return -1;
  return floor_log2 (x);
}

#endif /* GCC_VERSION < 3004 */

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

/* Arrange to dump core on error.  (The regular error message is still
   printed first, except in the case of abort().)  */

static void
setup_core_dumping (void)
{
#ifdef SIGABRT
  signal (SIGABRT, SIG_DFL);
#endif
#if defined(HAVE_SETRLIMIT)
  {
    struct rlimit rlim;
    if (getrlimit (RLIMIT_CORE, &rlim) != 0)
      fatal_error ("getting core file size maximum limit: %m");
    rlim.rlim_cur = rlim.rlim_max;
    if (setrlimit (RLIMIT_CORE, &rlim) != 0)
      fatal_error ("setting core file size limit to maximum: %m");
  }
#endif
  diagnostic_abort_on_error (global_dc);
}


/* Strip off a legitimate source ending from the input string NAME of
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by
   up to five characters.  (Java uses ".class".)  */

void
strip_off_ending (char *name, int len)
{
  int i;
  for (i = 2; i < 6 && len > i; i++)
    {
      if (name[len - i] == '.')
	{
	  name[len - i] = '\0';
	  break;
	}
    }
}

/* Output a quoted string.  */

void
output_quoted_string (FILE *asm_file, const char *string)
{
#ifdef OUTPUT_QUOTED_STRING
  OUTPUT_QUOTED_STRING (asm_file, string);
#else
  char c;

  putc ('\"', asm_file);
  while ((c = *string++) != 0)
    {
      if (ISPRINT (c))
	{
	  if (c == '\"' || c == '\\')
	    putc ('\\', asm_file);
	  putc (c, asm_file);
	}
      else
	fprintf (asm_file, "\\%03o", (unsigned char) c);
    }
  putc ('\"', asm_file);
#endif
}

/* Output a file name in the form wanted by System V.  */

void
output_file_directive (FILE *asm_file, const char *input_name)
{
  int len;
  const char *na;

  if (input_name == NULL)
    input_name = "<stdin>";
  else
    input_name = remap_debug_filename (input_name);

  len = strlen (input_name);
  na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (IS_DIR_SEPARATOR (na[-1]))
	break;
      na--;
    }

#ifdef ASM_OUTPUT_SOURCE_FILENAME
  ASM_OUTPUT_SOURCE_FILENAME (asm_file, na);
#else
  fprintf (asm_file, "\t.file\t");
  output_quoted_string (asm_file, na);
  putc ('\n', asm_file);
#endif
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
  if (TREE_ASM_WRITTEN (decl) || DECL_EXTERNAL (decl))
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
      node = varpool_node (decl);

      if (node->finalized)
	needed = false;
      else if (node->alias)
	needed = false;
      else if (!cgraph_global_info_ready
	       && (TREE_USED (decl)
		   || TREE_USED (DECL_ASSEMBLER_NAME (decl))))
	/* needed */;
      else if (node->needed)
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
      assemble_external (decl);
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
  if (errorcount != 0 || sorrycount != 0)
    return;

  timevar_push (TV_SYMOUT);
  for (i = 0; i < len; i++)
    debug_hooks->global_decl (vec[i]);
  timevar_pop (TV_SYMOUT);
}

/* Warn about a use of an identifier which was marked deprecated.  */
void
warn_deprecated_use (tree node, tree attr)
{
  const char *msg;

  if (node == 0 || !warn_deprecated_decl)
    return;

  if (!attr)
    {
      if (DECL_P (node))
	attr = DECL_ATTRIBUTES (node);
      else if (TYPE_P (node))
	{
	  tree decl = TYPE_STUB_DECL (node);
	  if (decl)
	    attr = lookup_attribute ("deprecated",
				     TYPE_ATTRIBUTES (TREE_TYPE (decl)));
	}
    }

  if (attr)
    attr = lookup_attribute ("deprecated", attr);

  if (attr)
    msg = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr)));
  else
    msg = NULL;

  if (DECL_P (node))
    {
      expanded_location xloc = expand_location (DECL_SOURCE_LOCATION (node));
      if (msg)
	warning (OPT_Wdeprecated_declarations,
		 "%qD is deprecated (declared at %s:%d): %s",
		 node, xloc.file, xloc.line, msg);
      else
	warning (OPT_Wdeprecated_declarations,
		 "%qD is deprecated (declared at %s:%d)",
		 node, xloc.file, xloc.line);
    }
  else if (TYPE_P (node))
    {
      tree what = NULL_TREE;
      tree decl = TYPE_STUB_DECL (node);

      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    what = TYPE_NAME (node);
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    what = DECL_NAME (TYPE_NAME (node));
	}

      if (decl)
	{
	  expanded_location xloc
	    = expand_location (DECL_SOURCE_LOCATION (decl));
	  if (what)
	    {
	      if (msg)
		warning (OPT_Wdeprecated_declarations,
			 "%qE is deprecated (declared at %s:%d): %s",
			 what, xloc.file, xloc.line, msg);
	      else
		warning (OPT_Wdeprecated_declarations,
			 "%qE is deprecated (declared at %s:%d)", what,
			 xloc.file, xloc.line);
	    }
	  else
	    {
	      if (msg)
		warning (OPT_Wdeprecated_declarations,
			 "type is deprecated (declared at %s:%d): %s",
			 xloc.file, xloc.line, msg);
	      else
		warning (OPT_Wdeprecated_declarations,
			 "type is deprecated (declared at %s:%d)",
			 xloc.file, xloc.line);
	    }
	}
      else
	{
	  if (what)
	    {
	      if (msg)
		warning (OPT_Wdeprecated_declarations, "%qE is deprecated: %s",
			 what, msg);
	      else
		warning (OPT_Wdeprecated_declarations, "%qE is deprecated", what);
	    }
	  else
	    {
	      if (msg)
		warning (OPT_Wdeprecated_declarations, "type is deprecated: %s",
			 msg);
	      else
		warning (OPT_Wdeprecated_declarations, "type is deprecated");
	    }
	}
    }
}

/* Compile an entire translation unit.  Write a file of assembly
   output and various debugging dumps.  */

static void
compile_file (void)
{
  /* Initialize yet another pass.  */

  ggc_protect_identifiers = true;

  init_cgraph ();
  init_final (main_input_filename);
  coverage_init (aux_base_name);
  statistics_init ();
  invoke_plugin_callbacks (PLUGIN_START_UNIT, NULL);

  timevar_push (TV_PARSE);

  /* Call the parser, which parses the entire file (calling
     rest_of_compilation for each function).  */
  lang_hooks.parse_file (set_yydebug);

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */
  timevar_pop (TV_PARSE);

  if (flag_syntax_only)
    return;

  ggc_protect_identifiers = false;

  /* This must also call cgraph_finalize_compilation_unit.  */
  lang_hooks.decls.final_write_globals ();

  if (errorcount || sorrycount)
    return;

  varpool_assemble_pending_decls ();
  finish_aliases_2 ();

  /* Likewise for mudflap static object registrations.  */
  if (flag_mudflap)
    mudflap_finish_file ();

  /* Likewise for emulated thread-local storage.  */
  if (!targetm.have_tls)
    emutls_finish ();

  output_shared_constant_pool ();
  output_object_blocks ();

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
    }

  /* Attach a special .ident directive to the end of the file to identify
     the version of GCC which compiled this code.  The format of the .ident
     string is patterned after the ones produced by native SVR4 compilers.  */
#ifdef IDENT_ASM_OP
  if (!flag_no_ident)
    {
      const char *pkg_version = "(GNU) ";

      if (strcmp ("(GCC) ", pkgversion_string))
	pkg_version = pkgversion_string;
      fprintf (asm_out_file, "%s\"GCC: %s%s\"\n",
	       IDENT_ASM_OP, pkg_version, version_string);
    }
#endif

  /* Invoke registered plugin callbacks.  */
  invoke_plugin_callbacks (PLUGIN_FINISH_UNIT, NULL);

  /* This must be at the end.  Some target ports emit end of file directives
     into the assembly file here, and hence we can not output anything to the
     assembly file after this point.  */
  targetm.asm_out.file_end ();
}

/* Parse a -d... command line switch.  */

void
decode_d_option (const char *arg)
{
  int c;

  while (*arg)
    switch (c = *arg++)
      {
      case 'A':
	flag_debug_asm = 1;
	break;
      case 'p':
	flag_print_asm_name = 1;
	break;
      case 'P':
	flag_dump_rtl_in_asm = 1;
	flag_print_asm_name = 1;
	break;
      case 'v':
	graph_dump_format = vcg;
	break;
      case 'x':
	rtl_dump_and_exit = 1;
	break;
      case 'y':
	set_yydebug = 1;
	break;
      case 'D':	/* These are handled by the preprocessor.  */
      case 'I':
      case 'M':
      case 'N':
      case 'U':
	break;
      case 'H':
	setup_core_dumping();
	break;
      case 'a':
	enable_rtl_dump_file ();
	break;

      default:
	  warning (0, "unrecognized gcc debugging option: %c", c);
	break;
      }
}

/* Indexed by enum debug_info_type.  */
const char *const debug_type_names[] =
{
  "none", "stabs", "coff", "dwarf-2", "xcoff", "vms"
};

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

#ifdef ASM_COMMENT_START
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
#endif

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
  const char **p;

  /* Fill in the -frandom-seed option, if the user didn't pass it, so
     that it can be printed below.  This helps reproducibility.  */
  if (!flag_random_seed)
    init_random_seed ();

  /* Print the options as passed.  */
  pos = print_single_switch (print_fn, pos,
			     SWITCH_TYPE_DESCRIPTIVE, _("options passed: "));

  for (p = &save_argv[1]; *p != NULL; p++)
    {
      if (**p == '-')
	{
	  /* Ignore these.  */
	  if (strcmp (*p, "-o") == 0
	      || strcmp (*p, "-dumpbase") == 0
	      || strcmp (*p, "-dumpdir") == 0
	      || strcmp (*p, "-auxbase") == 0)
	    {
	      if (p[1] != NULL)
		p++;
	      continue;
	    }

	  if (strcmp (*p, "-quiet") == 0
	      || strcmp (*p, "-version") == 0)
	    continue;

	  if ((*p)[1] == 'd')
	    continue;
	}

      pos = print_single_switch (print_fn, pos, SWITCH_TYPE_PASSED, *p);
    }

  if (pos > 0)
    print_fn (SWITCH_TYPE_LINE_END, NULL);

  /* Print the -f and -m options that have been enabled.
     We don't handle language specific options but printing argv
     should suffice.  */
  pos = print_single_switch (print_fn, 0,
			     SWITCH_TYPE_DESCRIPTIVE, _("options enabled: "));

  for (j = 0; j < cl_options_count; j++)
    if ((cl_options[j].flags & CL_REPORT)
	&& option_enabled (j) > 0)
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
	asm_out_file = fopen (asm_file_name, "w+b");
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

#ifdef ASM_COMMENT_START
      if (flag_verbose_asm)
	{
	  /* Print the list of switches in effect
	     into the assembler file as comments.  */
	  print_version (asm_out_file, ASM_COMMENT_START);
	  print_switch_values (print_to_asm_out_file);
	  putc ('\n', asm_out_file);
	}
#endif
    }
}

/* Return true if the state of option OPTION should be stored in PCH files
   and checked by default_pch_valid_p.  Store the option's current state
   in STATE if so.  */

static inline bool
option_affects_pch_p (int option, struct cl_option_state *state)
{
  if ((cl_options[option].flags & CL_TARGET) == 0)
    return false;
  if (cl_options[option].flag_var == &target_flags)
    if (targetm.check_pch_target_flags)
      return false;
  return get_option_state (option, state);
}

/* Default version of get_pch_validity.
   By default, every flag difference is fatal; that will be mostly right for
   most targets, but completely right for very few.  */

void *
default_get_pch_validity (size_t *sz)
{
  struct cl_option_state state;
  size_t i;
  char *result, *r;

  *sz = 2;
  if (targetm.check_pch_target_flags)
    *sz += sizeof (target_flags);
  for (i = 0; i < cl_options_count; i++)
    if (option_affects_pch_p (i, &state))
      *sz += state.size;

  result = r = XNEWVEC (char, *sz);
  r[0] = flag_pic;
  r[1] = flag_pie;
  r += 2;
  if (targetm.check_pch_target_flags)
    {
      memcpy (r, &target_flags, sizeof (target_flags));
      r += sizeof (target_flags);
    }

  for (i = 0; i < cl_options_count; i++)
    if (option_affects_pch_p (i, &state))
      {
	memcpy (r, state.data, state.size);
	r += state.size;
      }

  return result;
}

/* Return a message which says that a PCH file was created with a different
   setting of OPTION.  */

static const char *
pch_option_mismatch (const char *option)
{
  char *r;

  asprintf (&r, _("created and used with differing settings of '%s'"), option);
  if (r == NULL)
    return _("out of memory");
  return r;
}

/* Default version of pch_valid_p.  */

const char *
default_pch_valid_p (const void *data_p, size_t len)
{
  struct cl_option_state state;
  const char *data = (const char *)data_p;
  size_t i;

  /* -fpic and -fpie also usually make a PCH invalid.  */
  if (data[0] != flag_pic)
    return _("created and used with different settings of -fpic");
  if (data[1] != flag_pie)
    return _("created and used with different settings of -fpie");
  data += 2;

  /* Check target_flags.  */
  if (targetm.check_pch_target_flags)
    {
      int tf;
      const char *r;

      memcpy (&tf, data, sizeof (target_flags));
      data += sizeof (target_flags);
      len -= sizeof (target_flags);
      r = targetm.check_pch_target_flags (tf);
      if (r != NULL)
	return r;
    }

  for (i = 0; i < cl_options_count; i++)
    if (option_affects_pch_p (i, &state))
      {
	if (memcmp (data, state.data, state.size) != 0)
	  return pch_option_mismatch (cl_options[i].opt_text);
	data += state.size;
	len -= state.size;
      }

  return NULL;
}

/* Default tree printer.   Handles declarations only.  */
bool
default_tree_printer (pretty_printer *pp, text_info *text, const char *spec,
		      int precision, bool wide, bool set_locus, bool hash)
{
  tree t;

  /* FUTURE: %+x should set the locus.  */
  if (precision != 0 || wide || hash)
    return false;

  switch (*spec)
    {
    case 'E':
      t = va_arg (*text->args_ptr, tree);
      if (TREE_CODE (t) == IDENTIFIER_NODE)
	{
	  pp_identifier (pp, IDENTIFIER_POINTER (t));
	  return true;
	}
      break;

    case 'D':
      t = va_arg (*text->args_ptr, tree);
      if (DECL_DEBUG_EXPR_IS_FROM (t) && DECL_DEBUG_EXPR (t))
	t = DECL_DEBUG_EXPR (t);
      break;

    case 'F':
    case 'T':
      t = va_arg (*text->args_ptr, tree);
      break;

    default:
      return false;
    }

  if (set_locus && text->locus)
    *text->locus = DECL_SOURCE_LOCATION (t);

  if (DECL_P (t))
    {
      const char *n = DECL_NAME (t)
        ? identifier_to_locale (lang_hooks.decl_printable_name (t, 2))
        : _("<anonymous>");
      pp_string (pp, n);
    }
  else
    dump_generic_node (pp, t, 0, TDF_DIAGNOSTIC, 0);

  return true;
}

/* A helper function; used as the reallocator function for cpp's line
   table.  */
static void *
realloc_for_line_map (void *ptr, size_t len)
{
  return ggc_realloc (ptr, len);
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

  /* Initialize the diagnostics reporting machinery, so option parsing
     can give warnings and errors.  */
  diagnostic_initialize (global_dc);
  /* Set a default printer.  Language specific initializations will
     override it later.  */
  pp_format_decoder (global_dc->printer) = &default_tree_printer;

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
  line_table = GGC_NEW (struct line_maps);
  linemap_init (line_table);
  line_table->reallocator = realloc_for_line_map;
  init_ttree ();

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  /* Register the language-independent parameters.  */
  add_params (lang_independent_params, LAST_PARAM);

  /* This must be done after add_params but before argument processing.  */
  init_ggc_heuristics();
  init_optimization_passes ();
  statistics_early_init ();
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

  /* This replaces set_Wunused.  */
  if (warn_unused_function == -1)
    warn_unused_function = warn_unused;
  if (warn_unused_label == -1)
    warn_unused_label = warn_unused;
  /* Wunused-parameter is enabled if both -Wunused -Wextra are enabled.  */
  if (warn_unused_parameter == -1)
    warn_unused_parameter = (warn_unused && extra_warnings);
  if (warn_unused_variable == -1)
    warn_unused_variable = warn_unused;
  if (warn_unused_value == -1)
    warn_unused_value = warn_unused;

  /* This replaces set_Wextra.  */
  if (warn_uninitialized == -1)
    warn_uninitialized = extra_warnings;

  /* Allow the front end to perform consistency checks and do further
     initialization based on the command line options.  This hook also
     sets the original filename if appropriate (e.g. foo.i -> foo.c)
     so we can correctly initialize debug output.  */
  no_backend = lang_hooks.post_options (&main_input_filename);

#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

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
      || flag_loop_block
      || flag_loop_interchange
      || flag_loop_strip_mine
      || flag_graphite_identity
      || flag_loop_parallelize_all)
    sorry ("Graphite loop optimizations cannot be used");
#endif

  /* Unrolling all loops implies that standard loop unrolling must also
     be done.  */
  if (flag_unroll_all_loops)
    flag_unroll_loops = 1;

  /* The loop unrolling code assumes that cse will be run after loop.
     web and rename-registers also help when run after loop unrolling.  */
  if (flag_rerun_cse_after_loop == AUTODETECT_VALUE)
    flag_rerun_cse_after_loop = flag_unroll_loops || flag_peel_loops;

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
	  error ("could not open final insn dump file %qs: %s",
		 flag_dump_final_insns, strerror (errno));
	  flag_dump_final_insns = NULL;
	}
      else if (fclose (final_output))
	{
	  error ("could not close zeroed insn dump file %qs: %s",
		 flag_dump_final_insns, strerror (errno));
	  flag_dump_final_insns = NULL;
	}
    }

  /* Unless over-ridden for the target, assume that all DWARF levels
     may be emitted, if DWARF2_DEBUG is selected.  */
  if (dwarf_strict < 0)
    dwarf_strict = 0;

  /* A lot of code assumes write_symbols == NO_DEBUG if the debugging
     level is 0.  */
  if (debug_info_level == DINFO_LEVEL_NONE)
    write_symbols = NO_DEBUG;

  /* Now we know write_symbols, set up the debug hooks based on it.
     By default we do nothing for debug output.  */
  if (PREFERRED_DEBUGGING_TYPE == NO_DEBUG)
    default_debug_hooks = &do_nothing_debug_hooks;
#if defined(DBX_DEBUGGING_INFO)
  else if (PREFERRED_DEBUGGING_TYPE == DBX_DEBUG)
    default_debug_hooks = &dbx_debug_hooks;
#endif
#if defined(XCOFF_DEBUGGING_INFO)
  else if (PREFERRED_DEBUGGING_TYPE == XCOFF_DEBUG)
    default_debug_hooks = &xcoff_debug_hooks;
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (PREFERRED_DEBUGGING_TYPE == SDB_DEBUG)
    default_debug_hooks = &sdb_debug_hooks;
#endif
#ifdef DWARF2_DEBUGGING_INFO
  else if (PREFERRED_DEBUGGING_TYPE == DWARF2_DEBUG)
    default_debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef VMS_DEBUGGING_INFO
  else if (PREFERRED_DEBUGGING_TYPE == VMS_DEBUG
	   || PREFERRED_DEBUGGING_TYPE == VMS_AND_DWARF2_DEBUG)
    default_debug_hooks = &vmsdbg_debug_hooks;
#endif

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

  if (! targetm.have_named_sections)
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
  if (flag_prefetch_loop_arrays)
    {
      warning (0, "-fprefetch-loop-arrays not supported for this target");
      flag_prefetch_loop_arrays = 0;
    }
#else
  if (flag_prefetch_loop_arrays && !HAVE_prefetch)
    {
      warning (0, "-fprefetch-loop-arrays not supported for this target (try -march switches)");
      flag_prefetch_loop_arrays = 0;
    }
#endif

  /* This combination of options isn't handled for i386 targets and doesn't
     make much sense anyway, so don't allow it.  */
  if (flag_prefetch_loop_arrays && optimize_size)
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

  /* We may need to recompute regno_save_code[] and regno_restore_code[]
     after a mode change as well.  */
  if (flag_caller_saves)
    init_caller_save ();
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

  init_asm_output (name);

  /* This creates various _DECL nodes, so needs to be called after the
     front end is initialized.  */
  init_eh ();

  /* Do the target-specific parts of the initialization.  */
  lang_dependent_init_target ();

  /* If dbx symbol table desired, initialize writing it and output the
     predefined types.  */
  timevar_push (TV_SYMOUT);

#if defined DWARF2_DEBUGGING_INFO || defined DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_init ();
#endif

  /* Now we have the correct original filename, we can initialize
     debug output.  */
  (*debug_hooks->init) (name);

  timevar_pop (TV_SYMOUT);

  return 1;
}


/* Reinitialize everything when target parameters, such as register usage,
   have changed.  */
void
target_reinit (void)
{
  /* Reinitialize RTL backend.  */
  backend_init_target ();

  /* Reinitialize lang-dependent parts.  */
  lang_dependent_init_target ();
}

void
dump_memory_report (bool final)
{
  ggc_print_statistics ();
  stringpool_statistics ();
  dump_tree_statistics ();
  dump_gimple_statistics ();
  dump_rtx_statistics ();
  dump_varray_statistics ();
  dump_alloc_pool_statistics ();
  dump_bitmap_statistics ();
  dump_vec_loc_statistics ();
  dump_ggc_loc_statistics (final);
  dump_alias_stats (stderr);
  dump_pta_stats (stderr);
}

/* Clean up: close opened files, etc.  */

static void
finalize (void)
{
  /* Close the dump files.  */
  if (flag_gen_aux_info)
    {
      fclose (aux_info_file);
      if (errorcount)
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
      if (flag_wpa)
	unlink_if_ordinary (asm_file_name);
    }

  statistics_fini ();
  finish_optimization_passes ();

  ira_finish_once ();

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
  if (!errorcount)
    {
      /* This must be run always, because it is needed to compute the FP
	 predefined macros, such as __LDBL_MAX__, for targets using non
	 default FP formats.  */
      init_adjust_machine_modes ();

      /* Set up the back-end if requested.  */
      if (!no_backend)
	backend_init ();

      /* Language-dependent initialization.  Returns true on success.  */
      if (lang_dependent_init (main_input_filename))
	compile_file ();

      finalize ();
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
  expandargv (&argc, &argv);

  save_argv = CONST_CAST2 (const char **, char **, argv);

  /* Initialization of GCC's environment, and diagnostics.  */
  general_init (argv[0]);

  /* Parse the options and do minimal processing; basically just
     enough to default flags appropriately.  */
  decode_options (argc, CONST_CAST2 (const char **, char **, argv));

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

  /* Invoke registered plugin callbacks if any.  */
  invoke_plugin_callbacks (PLUGIN_FINISH, NULL);

  finalize_plugins ();
  if (errorcount || sorrycount)
    return (FATAL_EXIT_CODE);

  return (SUCCESS_EXIT_CODE);
}
