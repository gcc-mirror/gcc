/* Compiler driver program that can handle many languages.
   Copyright (C) 1987-2013 Free Software Foundation, Inc.

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

/* This program is the user interface to the C compiler and possibly to
other compilers.  It is used because compilation is a complicated procedure
which involves running several programs and passing temporary files between
them, forwarding the users switches to those programs selectively,
and deleting the temporary files at the end.

CC recognizes how to compile each input file by suffixes in the file names.
Once it knows which kind of compilation to perform, the procedure for
compilation is specified by a string called a "spec".  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "multilib.h" /* before tm.h */
#include "tm.h"
#include "xregex.h"
#include "obstack.h"
#include "intl.h"
#include "prefix.h"
#include "gcc.h"
#include "diagnostic.h"
#include "flags.h"
#include "opts.h"
#include "params.h"
#include "vec.h"
#include "filenames.h"

/* By default there is no special suffix for target executables.  */
/* FIXME: when autoconf is fixed, remove the host check - dj */
#if defined(TARGET_EXECUTABLE_SUFFIX) && defined(HOST_EXECUTABLE_SUFFIX)
#define HAVE_TARGET_EXECUTABLE_SUFFIX
#endif

/* By default there is no special suffix for host executables.  */
#ifdef HOST_EXECUTABLE_SUFFIX
#define HAVE_HOST_EXECUTABLE_SUFFIX
#else
#define HOST_EXECUTABLE_SUFFIX ""
#endif

/* By default, the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif

static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };

/* Most every one is fine with LIBRARY_PATH.  For some, it conflicts.  */
#ifndef LIBRARY_PATH_ENV
#define LIBRARY_PATH_ENV "LIBRARY_PATH"
#endif

/* If a stage of compilation returns an exit status >= 1,
   compilation of that file ceases.  */

#define MIN_FATAL_STATUS 1

/* Flag set by cppspec.c to 1.  */
int is_cpp_driver;

/* Flag set to nonzero if an @file argument has been supplied to gcc.  */
static bool at_file_supplied;

/* Definition of string containing the arguments given to configure.  */
#include "configargs.h"

/* Flag saying to print the command line options understood by gcc and its
   sub-processes.  */

static int print_help_list;

/* Flag saying to print the version of gcc and its sub-processes.  */

static int print_version;

/* Flag indicating whether we should ONLY print the command and
   arguments (like verbose_flag) without executing the command.
   Displayed arguments are quoted so that the generated command
   line is suitable for execution.  This is intended for use in
   shell scripts to capture the driver-generated command line.  */
static int verbose_only_flag;

/* Flag indicating how to print command line options of sub-processes.  */

static int print_subprocess_help;

/* Whether we should report subprocess execution times to a file.  */

FILE *report_times_to_file = NULL;

/* Nonzero means place this string before uses of /, so that include
   and library files can be found in an alternate location.  */

#ifdef TARGET_SYSTEM_ROOT
static const char *target_system_root = TARGET_SYSTEM_ROOT;
#else
static const char *target_system_root = 0;
#endif

/* Nonzero means pass the updated target_system_root to the compiler.  */

static int target_system_root_changed;

/* Nonzero means append this string to target_system_root.  */

static const char *target_sysroot_suffix = 0;

/* Nonzero means append this string to target_system_root for headers.  */

static const char *target_sysroot_hdrs_suffix = 0;

/* Nonzero means write "temp" files in source directory
   and use the source file's name in them, and don't delete them.  */

static enum save_temps {
  SAVE_TEMPS_NONE,		/* no -save-temps */
  SAVE_TEMPS_CWD,		/* -save-temps in current directory */
  SAVE_TEMPS_OBJ		/* -save-temps in object directory */
} save_temps_flag;

/* Output file to use to get the object directory for -save-temps=obj  */
static char *save_temps_prefix = 0;
static size_t save_temps_length = 0;

/* The compiler version.  */

static const char *compiler_version;

/* The target version.  */

static const char *const spec_version = DEFAULT_TARGET_VERSION;

/* The target machine.  */

static const char *spec_machine = DEFAULT_TARGET_MACHINE;

/* Nonzero if cross-compiling.
   When -b is used, the value comes from the `specs' file.  */

#ifdef CROSS_DIRECTORY_STRUCTURE
static const char *cross_compile = "1";
#else
static const char *cross_compile = "0";
#endif

/* Greatest exit code of sub-processes that has been encountered up to
   now.  */
static int greatest_status = 1;

/* This is the obstack which we use to allocate many strings.  */

static struct obstack obstack;

/* This is the obstack to build an environment variable to pass to
   collect2 that describes all of the relevant switches of what to
   pass the compiler in building the list of pointers to constructors
   and destructors.  */

static struct obstack collect_obstack;

/* Forward declaration for prototypes.  */
struct path_prefix;
struct prefix_list;

static void init_spec (void);
static void store_arg (const char *, int, int);
static void insert_wrapper (const char *);
static char *load_specs (const char *);
static void read_specs (const char *, bool, bool);
static void set_spec (const char *, const char *, bool);
static struct compiler *lookup_compiler (const char *, size_t, const char *);
static char *build_search_list (const struct path_prefix *, const char *,
				bool, bool);
static void xputenv (const char *);
static void putenv_from_prefixes (const struct path_prefix *, const char *,
				  bool);
static int access_check (const char *, int);
static char *find_a_file (const struct path_prefix *, const char *, int, bool);
static void add_prefix (struct path_prefix *, const char *, const char *,
			int, int, int);
static void add_sysrooted_prefix (struct path_prefix *, const char *,
				  const char *, int, int, int);
static char *skip_whitespace (char *);
static void delete_if_ordinary (const char *);
static void delete_temp_files (void);
static void delete_failure_queue (void);
static void clear_failure_queue (void);
static int check_live_switch (int, int);
static const char *handle_braces (const char *);
static inline bool input_suffix_matches (const char *, const char *);
static inline bool switch_matches (const char *, const char *, int);
static inline void mark_matching_switches (const char *, const char *, int);
static inline void process_marked_switches (void);
static const char *process_brace_body (const char *, const char *, const char *, int, int);
static const struct spec_function *lookup_spec_function (const char *);
static const char *eval_spec_function (const char *, const char *);
static const char *handle_spec_function (const char *);
static char *save_string (const char *, int);
static void set_collect_gcc_options (void);
static int do_spec_1 (const char *, int, const char *);
static int do_spec_2 (const char *);
static void do_option_spec (const char *, const char *);
static void do_self_spec (const char *);
static const char *find_file (const char *);
static int is_directory (const char *, bool);
static const char *validate_switches (const char *, bool);
static void validate_all_switches (void);
static inline void validate_switches_from_spec (const char *, bool);
static void give_switch (int, int);
static int used_arg (const char *, int);
static int default_arg (const char *, int);
static void set_multilib_dir (void);
static void print_multilib_info (void);
static void perror_with_name (const char *);
static void display_help (void);
static void add_preprocessor_option (const char *, int);
static void add_assembler_option (const char *, int);
static void add_linker_option (const char *, int);
static void process_command (unsigned int, struct cl_decoded_option *);
static int execute (void);
static void alloc_args (void);
static void clear_args (void);
static void fatal_signal (int);
#if defined(ENABLE_SHARED_LIBGCC) && !defined(REAL_LIBGCC_SPEC)
static void init_gcc_specs (struct obstack *, const char *, const char *,
			    const char *);
#endif
#if defined(HAVE_TARGET_OBJECT_SUFFIX) || defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
static const char *convert_filename (const char *, int, int);
#endif

static const char *getenv_spec_function (int, const char **);
static const char *if_exists_spec_function (int, const char **);
static const char *if_exists_else_spec_function (int, const char **);
static const char *replace_outfile_spec_function (int, const char **);
static const char *remove_outfile_spec_function (int, const char **);
static const char *version_compare_spec_function (int, const char **);
static const char *include_spec_function (int, const char **);
static const char *find_file_spec_function (int, const char **);
static const char *find_plugindir_spec_function (int, const char **);
static const char *print_asm_header_spec_function (int, const char **);
static const char *compare_debug_dump_opt_spec_function (int, const char **);
static const char *compare_debug_self_opt_spec_function (int, const char **);
static const char *compare_debug_auxbase_opt_spec_function (int, const char **);
static const char *pass_through_libs_spec_func (int, const char **);
static const char *replace_extension_spec_func (int, const char **);
static char *convert_white_space (char *);

/* The Specs Language

Specs are strings containing lines, each of which (if not blank)
is made up of a program name, and arguments separated by spaces.
The program name must be exact and start from root, since no path
is searched and it is unreliable to depend on the current working directory.
Redirection of input or output is not supported; the subprograms must
accept filenames saying what files to read and write.

In addition, the specs can contain %-sequences to substitute variable text
or for conditional text.  Here is a table of all defined %-sequences.
Note that spaces are not generated automatically around the results of
expanding these sequences; therefore, you can concatenate them together
or with constant text in a single argument.

 %%	substitute one % into the program name or argument.
 %i     substitute the name of the input file being processed.
 %b     substitute the basename of the input file being processed.
	This is the substring up to (and not including) the last period
	and not including the directory unless -save-temps was specified
	to put temporaries in a different location.
 %B	same as %b, but include the file suffix (text after the last period).
 %gSUFFIX
	substitute a file name that has suffix SUFFIX and is chosen
	once per compilation, and mark the argument a la %d.  To reduce
	exposure to denial-of-service attacks, the file name is now
	chosen in a way that is hard to predict even when previously
	chosen file names are known.  For example, `%g.s ... %g.o ... %g.s'
	might turn into `ccUVUUAU.s ccXYAXZ12.o ccUVUUAU.s'.  SUFFIX matches
	the regexp "[.0-9A-Za-z]*%O"; "%O" is treated exactly as if it
	had been pre-processed.  Previously, %g was simply substituted
	with a file name chosen once per compilation, without regard
	to any appended suffix (which was therefore treated just like
	ordinary text), making such attacks more likely to succeed.
 %|SUFFIX
	like %g, but if -pipe is in effect, expands simply to "-".
 %mSUFFIX
        like %g, but if -pipe is in effect, expands to nothing.  (We have both
	%| and %m to accommodate differences between system assemblers; see
	the AS_NEEDS_DASH_FOR_PIPED_INPUT target macro.)
 %uSUFFIX
	like %g, but generates a new temporary file name even if %uSUFFIX
	was already seen.
 %USUFFIX
	substitutes the last file name generated with %uSUFFIX, generating a
	new one if there is no such last file name.  In the absence of any
	%uSUFFIX, this is just like %gSUFFIX, except they don't share
	the same suffix "space", so `%g.s ... %U.s ... %g.s ... %U.s'
	would involve the generation of two distinct file names, one
	for each `%g.s' and another for each `%U.s'.  Previously, %U was
	simply substituted with a file name chosen for the previous %u,
	without regard to any appended suffix.
 %jSUFFIX
        substitutes the name of the HOST_BIT_BUCKET, if any, and if it is
        writable, and if save-temps is off; otherwise, substitute the name
        of a temporary file, just like %u.  This temporary file is not
        meant for communication between processes, but rather as a junk
        disposal mechanism.
 %.SUFFIX
        substitutes .SUFFIX for the suffixes of a matched switch's args when
        it is subsequently output with %*. SUFFIX is terminated by the next
        space or %.
 %d	marks the argument containing or following the %d as a
	temporary file name, so that that file will be deleted if GCC exits
	successfully.  Unlike %g, this contributes no text to the argument.
 %w	marks the argument containing or following the %w as the
	"output file" of this compilation.  This puts the argument
	into the sequence of arguments that %o will substitute later.
 %V	indicates that this compilation produces no "output file".
 %W{...}
	like %{...} but mark last argument supplied within
	as a file to be deleted on failure.
 %o	substitutes the names of all the output files, with spaces
	automatically placed around them.  You should write spaces
	around the %o as well or the results are undefined.
	%o is for use in the specs for running the linker.
	Input files whose names have no recognized suffix are not compiled
	at all, but they are included among the output files, so they will
	be linked.
 %O	substitutes the suffix for object files.  Note that this is
        handled specially when it immediately follows %g, %u, or %U
	(with or without a suffix argument) because of the need for
	those to form complete file names.  The handling is such that
	%O is treated exactly as if it had already been substituted,
	except that %g, %u, and %U do not currently support additional
	SUFFIX characters following %O as they would following, for
	example, `.o'.
 %I	Substitute any of -iprefix (made from GCC_EXEC_PREFIX), -isysroot
	(made from TARGET_SYSTEM_ROOT), -isystem (made from COMPILER_PATH
	and -B options) and -imultilib as necessary.
 %s     current argument is the name of a library or startup file of some sort.
        Search for that file in a standard list of directories
	and substitute the full name found.
 %eSTR  Print STR as an error message.  STR is terminated by a newline.
        Use this when inconsistent options are detected.
 %nSTR  Print STR as a notice.  STR is terminated by a newline.
 %x{OPTION}	Accumulate an option for %X.
 %X	Output the accumulated linker options specified by compilations.
 %Y	Output the accumulated assembler options specified by compilations.
 %Z	Output the accumulated preprocessor options specified by compilations.
 %a     process ASM_SPEC as a spec.
        This allows config.h to specify part of the spec for running as.
 %A	process ASM_FINAL_SPEC as a spec.  A capital A is actually
	used here.  This can be used to run a post-processor after the
	assembler has done its job.
 %D	Dump out a -L option for each directory in startfile_prefixes.
	If multilib_dir is set, extra entries are generated with it affixed.
 %l     process LINK_SPEC as a spec.
 %L     process LIB_SPEC as a spec.
 %M     Output multilib_os_dir.
 %G     process LIBGCC_SPEC as a spec.
 %R     Output the concatenation of target_system_root and
        target_sysroot_suffix.
 %S     process STARTFILE_SPEC as a spec.  A capital S is actually used here.
 %E     process ENDFILE_SPEC as a spec.  A capital E is actually used here.
 %C     process CPP_SPEC as a spec.
 %1	process CC1_SPEC as a spec.
 %2	process CC1PLUS_SPEC as a spec.
 %*	substitute the variable part of a matched option.  (See below.)
	Note that each comma in the substituted string is replaced by
	a single space.
 %<S    remove all occurrences of -S from the command line.
        Note - this command is position dependent.  % commands in the
        spec string before this one will see -S, % commands in the
        spec string after this one will not.
 %>S	Similar to "%<S", but keep it in the GCC command line.
 %<S*	remove all occurrences of all switches beginning with -S from the
        command line.
 %:function(args)
	Call the named function FUNCTION, passing it ARGS.  ARGS is
	first processed as a nested spec string, then split into an
	argument vector in the usual fashion.  The function returns
	a string which is processed as if it had appeared literally
	as part of the current spec.
 %{S}   substitutes the -S switch, if that switch was given to GCC.
	If that switch was not specified, this substitutes nothing.
	Here S is a metasyntactic variable.
 %{S*}  substitutes all the switches specified to GCC whose names start
	with -S.  This is used for -o, -I, etc; switches that take
	arguments.  GCC considers `-o foo' as being one switch whose
	name starts with `o'.  %{o*} would substitute this text,
	including the space; thus, two arguments would be generated.
 %{S*&T*} likewise, but preserve order of S and T options (the order
	of S and T in the spec is not significant).  Can be any number
	of ampersand-separated variables; for each the wild card is
	optional.  Useful for CPP as %{D*&U*&A*}.

 %{S:X}   substitutes X, if the -S switch was given to GCC.
 %{!S:X}  substitutes X, if the -S switch was NOT given to GCC.
 %{S*:X}  substitutes X if one or more switches whose names start
          with -S was given to GCC.  Normally X is substituted only
          once, no matter how many such switches appeared.  However,
          if %* appears somewhere in X, then X will be substituted
          once for each matching switch, with the %* replaced by the
          part of that switch that matched the '*'.
 %{.S:X}  substitutes X, if processing a file with suffix S.
 %{!.S:X} substitutes X, if NOT processing a file with suffix S.
 %{,S:X}  substitutes X, if processing a file which will use spec S.
 %{!,S:X} substitutes X, if NOT processing a file which will use spec S.

 %{S|T:X} substitutes X if either -S or -T was given to GCC.  This may be
	  combined with '!', '.', ',', and '*' as above binding stronger
	  than the OR.
	  If %* appears in X, all of the alternatives must be starred, and
	  only the first matching alternative is substituted.
 %{S:X;   if S was given to GCC, substitutes X;
   T:Y;   else if T was given to GCC, substitutes Y;
    :D}   else substitutes D.  There can be as many clauses as you need.
          This may be combined with '.', '!', ',', '|', and '*' as above.

 %(Spec) processes a specification defined in a specs file as *Spec:

The conditional text X in a %{S:X} or similar construct may contain
other nested % constructs or spaces, or even newlines.  They are
processed as usual, as described above.  Trailing white space in X is
ignored.  White space may also appear anywhere on the left side of the
colon in these constructs, except between . or * and the corresponding
word.

The -O, -f, -g, -m, and -W switches are handled specifically in these
constructs.  If another value of -O or the negated form of a -f, -m, or
-W switch is found later in the command line, the earlier switch
value is ignored, except with {S*} where S is just one letter; this
passes all matching options.

The character | at the beginning of the predicate text is used to indicate
that a command should be piped to the following command, but only if -pipe
is specified.

Note that it is built into GCC which switches take arguments and which
do not.  You might think it would be useful to generalize this to
allow each compiler's spec to say which switches take arguments.  But
this cannot be done in a consistent fashion.  GCC cannot even decide
which input files have been specified without knowing which switches
take arguments, and it must know which input files to compile in order
to tell which compilers to run.

GCC also knows implicitly that arguments starting in `-l' are to be
treated as compiler output files, and passed to the linker in their
proper position among the other output files.  */

/* Define the macros used for specs %a, %l, %L, %S, %C, %1.  */

/* config.h can define ASM_SPEC to provide extra args to the assembler
   or extra switch-translations.  */
#ifndef ASM_SPEC
#define ASM_SPEC ""
#endif

/* config.h can define ASM_FINAL_SPEC to run a post processor after
   the assembler has run.  */
#ifndef ASM_FINAL_SPEC
#define ASM_FINAL_SPEC \
  "%{gsplit-dwarf: \n\
       objcopy --extract-dwo \
	 %{c:%{o*:%*}%{!o*:%b%O}}%{!c:%U%O} \
	 %{c:%{o*:%:replace-extension(%{o*:%*} .dwo)}%{!o*:%b.dwo}}%{!c:%b.dwo} \n\
       objcopy --strip-dwo \
	 %{c:%{o*:%*}%{!o*:%b%O}}%{!c:%U%O} \
    }"
#endif

/* config.h can define CPP_SPEC to provide extra args to the C preprocessor
   or extra switch-translations.  */
#ifndef CPP_SPEC
#define CPP_SPEC ""
#endif

/* config.h can define CC1_SPEC to provide extra args to cc1 and cc1plus
   or extra switch-translations.  */
#ifndef CC1_SPEC
#define CC1_SPEC ""
#endif

/* config.h can define CC1PLUS_SPEC to provide extra args to cc1plus
   or extra switch-translations.  */
#ifndef CC1PLUS_SPEC
#define CC1PLUS_SPEC ""
#endif

/* config.h can define LINK_SPEC to provide extra args to the linker
   or extra switch-translations.  */
#ifndef LINK_SPEC
#define LINK_SPEC ""
#endif

/* config.h can define LIB_SPEC to override the default libraries.  */
#ifndef LIB_SPEC
#define LIB_SPEC "%{!shared:%{g*:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"
#endif

/* mudflap specs */
#ifndef MFWRAP_SPEC
/* XXX: valid only for GNU ld */
/* XXX: should exactly match hooks provided by libmudflap.a */
#define MFWRAP_SPEC " %{static: %{fmudflap|fmudflapth: \
 --wrap=malloc --wrap=free --wrap=calloc --wrap=realloc\
 --wrap=mmap --wrap=mmap64 --wrap=munmap --wrap=alloca\
} %{fmudflapth: --wrap=pthread_create\
}} %{fmudflap|fmudflapth: --wrap=main}"
#endif
#ifndef MFLIB_SPEC
#define MFLIB_SPEC "%{fmudflap|fmudflapth: -export-dynamic}"
#endif

/* When using -fsplit-stack we need to wrap pthread_create, in order
   to initialize the stack guard.  We always use wrapping, rather than
   shared library ordering, and we keep the wrapper function in
   libgcc.  This is not yet a real spec, though it could become one;
   it is currently just stuffed into LINK_SPEC.  FIXME: This wrapping
   only works with GNU ld and gold.  FIXME: This is incompatible with
   -fmudflap when linking statically, which wants to do its own
   wrapping.  */
#define STACK_SPLIT_SPEC " %{fsplit-stack: --wrap=pthread_create}"

#ifndef LIBASAN_SPEC
#ifdef STATIC_LIBASAN_LIBS
#define ADD_STATIC_LIBASAN_LIBS \
  " %{static-libasan:" STATIC_LIBASAN_LIBS "}"
#else
#define ADD_STATIC_LIBASAN_LIBS
#endif
#ifdef LIBASAN_EARLY_SPEC
#define LIBASAN_SPEC ADD_STATIC_LIBASAN_LIBS
#elif defined(HAVE_LD_STATIC_DYNAMIC)
#define LIBASAN_SPEC "%{static-libasan:" LD_STATIC_OPTION \
		     "} -lasan %{static-libasan:" LD_DYNAMIC_OPTION "}" \
		     ADD_STATIC_LIBASAN_LIBS
#else
#define LIBASAN_SPEC "-lasan" ADD_STATIC_LIBASAN_LIBS
#endif
#endif

#ifndef LIBASAN_EARLY_SPEC
#define LIBASAN_EARLY_SPEC ""
#endif

#ifndef LIBTSAN_SPEC
#ifdef STATIC_LIBTSAN_LIBS
#define ADD_STATIC_LIBTSAN_LIBS \
  " %{static-libtsan:" STATIC_LIBTSAN_LIBS "}"
#else
#define ADD_STATIC_LIBTSAN_LIBS
#endif
#ifdef LIBTSAN_EARLY_SPEC
#define LIBTSAN_SPEC ADD_STATIC_LIBTSAN_LIBS
#elif defined(HAVE_LD_STATIC_DYNAMIC)
#define LIBTSAN_SPEC "%{static-libtsan:" LD_STATIC_OPTION \
		     "} -ltsan %{static-libtsan:" LD_DYNAMIC_OPTION "}" \
		     ADD_STATIC_LIBTSAN_LIBS
#else
#define LIBTSAN_SPEC "-ltsan" ADD_STATIC_LIBTSAN_LIBS
#endif
#endif

#ifndef LIBTSAN_EARLY_SPEC
#define LIBTSAN_EARLY_SPEC ""
#endif

/* config.h can define LIBGCC_SPEC to override how and when libgcc.a is
   included.  */
#ifndef LIBGCC_SPEC
#if defined(REAL_LIBGCC_SPEC)
#define LIBGCC_SPEC REAL_LIBGCC_SPEC
#elif defined(LINK_LIBGCC_SPECIAL_1)
/* Have gcc do the search for libgcc.a.  */
#define LIBGCC_SPEC "libgcc.a%s"
#else
#define LIBGCC_SPEC "-lgcc"
#endif
#endif

/* config.h can define STARTFILE_SPEC to override the default crt0 files.  */
#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}}"
#endif

/* config.h can define ENDFILE_SPEC to override the default crtn files.  */
#ifndef ENDFILE_SPEC
#define ENDFILE_SPEC ""
#endif

#ifndef LINKER_NAME
#define LINKER_NAME "collect2"
#endif

#ifdef HAVE_AS_DEBUG_PREFIX_MAP
#define ASM_MAP " %{fdebug-prefix-map=*:--debug-prefix-map %*}"
#else
#define ASM_MAP ""
#endif

/* Define ASM_DEBUG_SPEC to be a spec suitable for translating '-g'
   to the assembler.  */
#ifndef ASM_DEBUG_SPEC
# if defined(DBX_DEBUGGING_INFO) && defined(DWARF2_DEBUGGING_INFO) \
     && defined(HAVE_AS_GDWARF2_DEBUG_FLAG) && defined(HAVE_AS_GSTABS_DEBUG_FLAG)
#  define ASM_DEBUG_SPEC						\
      (PREFERRED_DEBUGGING_TYPE == DBX_DEBUG				\
       ? "%{!g0:%{gdwarf*:--gdwarf2}%{!gdwarf*:%{g*:--gstabs}}}" ASM_MAP	\
       : "%{!g0:%{gstabs*:--gstabs}%{!gstabs*:%{g*:--gdwarf2}}}" ASM_MAP)
# else
#  if defined(DBX_DEBUGGING_INFO) && defined(HAVE_AS_GSTABS_DEBUG_FLAG)
#   define ASM_DEBUG_SPEC "%{g*:%{!g0:--gstabs}}" ASM_MAP
#  endif
#  if defined(DWARF2_DEBUGGING_INFO) && defined(HAVE_AS_GDWARF2_DEBUG_FLAG)
#   define ASM_DEBUG_SPEC "%{g*:%{!g0:--gdwarf2}}" ASM_MAP
#  endif
# endif
#endif
#ifndef ASM_DEBUG_SPEC
# define ASM_DEBUG_SPEC ""
#endif

/* Here is the spec for running the linker, after compiling all files.  */

/* This is overridable by the target in case they need to specify the
   -lgcc and -lc order specially, yet not require them to override all
   of LINK_COMMAND_SPEC.  */
#ifndef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC "%G %L %G"
#endif

#ifndef LINK_SSP_SPEC
#ifdef TARGET_LIBC_PROVIDES_SSP
#define LINK_SSP_SPEC "%{fstack-protector:}"
#else
#define LINK_SSP_SPEC "%{fstack-protector|fstack-protector-strong|fstack-protector-all:-lssp_nonshared -lssp}"
#endif
#endif

#ifndef LINK_PIE_SPEC
#ifdef HAVE_LD_PIE
#define LINK_PIE_SPEC "%{pie:-pie} "
#else
#define LINK_PIE_SPEC "%{pie:} "
#endif
#endif

#ifndef LINK_BUILDID_SPEC
# if defined(HAVE_LD_BUILDID) && defined(ENABLE_LD_BUILDID)
#  define LINK_BUILDID_SPEC "%{!r:--build-id} "
# endif
#endif

/* Conditional to test whether the LTO plugin is used or not.
   FIXME: For slim LTO we will need to enable plugin unconditionally.  This
   still cause problems with PLUGIN_LD != LD and when plugin is built but
   not useable.  For GCC 4.6 we don't support slim LTO and thus we can enable
   plugin only when LTO is enabled.  We still honor explicit
   -fuse-linker-plugin if the linker used understands -plugin.  */

/* The linker has some plugin support.  */
#if HAVE_LTO_PLUGIN > 0
/* The linker used has full plugin support, use LTO plugin by default.  */
#if HAVE_LTO_PLUGIN == 2
#define PLUGIN_COND "!fno-use-linker-plugin:%{flto|flto=*|fuse-linker-plugin"
#define PLUGIN_COND_CLOSE "}"
#else
/* The linker used has limited plugin support, use LTO plugin with explicit
   -fuse-linker-plugin.  */
#define PLUGIN_COND "fuse-linker-plugin"
#define PLUGIN_COND_CLOSE ""
#endif
#define LINK_PLUGIN_SPEC \
    "%{"PLUGIN_COND": \
    -plugin %(linker_plugin_file) \
    -plugin-opt=%(lto_wrapper) \
    -plugin-opt=-fresolution=%u.res \
    %{!nostdlib:%{!nodefaultlibs:%:pass-through-libs(%(link_gcc_c_sequence))}} \
    }"PLUGIN_COND_CLOSE
#else
/* The linker used doesn't support -plugin, reject -fuse-linker-plugin.  */
#define LINK_PLUGIN_SPEC "%{fuse-linker-plugin:\
    %e-fuse-linker-plugin is not supported in this configuration}"
#endif

/* Linker command line options for -fsanitize= early on the command line.  */
#ifndef SANITIZER_EARLY_SPEC
#define SANITIZER_EARLY_SPEC "\
%{!nostdlib:%{!nodefaultlibs:%{fsanitize=address:" LIBASAN_EARLY_SPEC "} \
    %{fsanitize=thread:" LIBTSAN_EARLY_SPEC "}}}"
#endif

/* Linker command line options for -fsanitize= late on the command line.  */
#ifndef SANITIZER_SPEC
#define SANITIZER_SPEC "\
%{!nostdlib:%{!nodefaultlibs:%{fsanitize=address:" LIBASAN_SPEC "\
    %{static:%ecannot specify -static with -fsanitize=address}\
    %{fsanitize=thread:%e-fsanitize=address is incompatible with -fsanitize=thread}}\
    %{fsanitize=thread:" LIBTSAN_SPEC "\
    %{!pie:%{!shared:%e-fsanitize=thread linking must be done with -pie or -shared}}}}}"
#endif

/*  This is the spec to use, once the code for creating the vtable
    verification runtime library, libvtv.so, has been created.  Currently
    the vtable verification runtime functions are in libstdc++, so we use
    the spec just below this one.  */
#ifndef VTABLE_VERIFICATION_SPEC
#define VTABLE_VERIFICATION_SPEC "\
%{!nostdlib:%{fvtable-verify=std: -lvtv -u_vtable_map_vars_start -u_vtable_map_vars_end}\
    %{fvtable-verify=preinit: -lvtv -u_vtable_map_vars_start -u_vtable_map_vars_end}}"
#endif

/* -u* was put back because both BSD and SysV seem to support it.  */
/* %{static:} simply prevents an error message if the target machine
   doesn't handle -static.  */
/* We want %{T*} after %{L*} and %D so that it can be used to specify linker
   scripts which exist in user specified directories, or in standard
   directories.  */
/* We pass any -flto flags on to the linker, which is expected
   to understand them.  In practice, this means it had better be collect2.  */
/* %{e*} includes -export-dynamic; see comment in common.opt.  */
#ifndef LINK_COMMAND_SPEC
#define LINK_COMMAND_SPEC "\
%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %(linker) " \
    LINK_PLUGIN_SPEC \
   "%{flto|flto=*:%<fcompare-debug*} \
    %{flto} %{flto=*} %l " LINK_PIE_SPEC \
   "%{fuse-ld=*:-fuse-ld=%*}\
    %X %{o*} %{e*} %{N} %{n} %{r}\
    %{s} %{t} %{u*} %{z} %{Z} %{!nostdlib:%{!nostartfiles:%S}} " VTABLE_VERIFICATION_SPEC " \
    %{static:} %{L*} %(mfwrap) %(link_libgcc) " SANITIZER_EARLY_SPEC " %o\
    %{fopenmp|ftree-parallelize-loops=*:%:include(libgomp.spec)%(link_gomp)}\
    %{fgnu-tm:%:include(libitm.spec)%(link_itm)}\
    %(mflib) " STACK_SPLIT_SPEC "\
    %{fprofile-arcs|fprofile-generate*|coverage:-lgcov} " SANITIZER_SPEC " \
    %{!nostdlib:%{!nodefaultlibs:%(link_ssp) %(link_gcc_c_sequence)}}\
    %{!nostdlib:%{!nostartfiles:%E}} %{T*} }}}}}}"
#endif

#ifndef LINK_LIBGCC_SPEC
/* Generate -L options for startfile prefix list.  */
# define LINK_LIBGCC_SPEC "%D"
#endif

#ifndef STARTFILE_PREFIX_SPEC
# define STARTFILE_PREFIX_SPEC ""
#endif

#ifndef SYSROOT_SPEC
# define SYSROOT_SPEC "--sysroot=%R"
#endif

#ifndef SYSROOT_SUFFIX_SPEC
# define SYSROOT_SUFFIX_SPEC ""
#endif

#ifndef SYSROOT_HEADERS_SUFFIX_SPEC
# define SYSROOT_HEADERS_SUFFIX_SPEC ""
#endif

static const char *asm_debug = ASM_DEBUG_SPEC;
static const char *cpp_spec = CPP_SPEC;
static const char *cc1_spec = CC1_SPEC;
static const char *cc1plus_spec = CC1PLUS_SPEC;
static const char *link_gcc_c_sequence_spec = LINK_GCC_C_SEQUENCE_SPEC;
static const char *link_ssp_spec = LINK_SSP_SPEC;
static const char *asm_spec = ASM_SPEC;
static const char *asm_final_spec = ASM_FINAL_SPEC;
static const char *link_spec = LINK_SPEC;
static const char *lib_spec = LIB_SPEC;
static const char *mfwrap_spec = MFWRAP_SPEC;
static const char *mflib_spec = MFLIB_SPEC;
static const char *link_gomp_spec = "";
static const char *libgcc_spec = LIBGCC_SPEC;
static const char *endfile_spec = ENDFILE_SPEC;
static const char *startfile_spec = STARTFILE_SPEC;
static const char *linker_name_spec = LINKER_NAME;
static const char *linker_plugin_file_spec = "";
static const char *lto_wrapper_spec = "";
static const char *lto_gcc_spec = "";
static const char *link_command_spec = LINK_COMMAND_SPEC;
static const char *link_libgcc_spec = LINK_LIBGCC_SPEC;
static const char *startfile_prefix_spec = STARTFILE_PREFIX_SPEC;
static const char *sysroot_spec = SYSROOT_SPEC;
static const char *sysroot_suffix_spec = SYSROOT_SUFFIX_SPEC;
static const char *sysroot_hdrs_suffix_spec = SYSROOT_HEADERS_SUFFIX_SPEC;
static const char *self_spec = "";

/* Standard options to cpp, cc1, and as, to reduce duplication in specs.
   There should be no need to override these in target dependent files,
   but we need to copy them to the specs file so that newer versions
   of the GCC driver can correctly drive older tool chains with the
   appropriate -B options.  */

/* When cpplib handles traditional preprocessing, get rid of this, and
   call cc1 (or cc1obj in objc/lang-specs.h) from the main specs so
   that we default the front end language better.  */
static const char *trad_capable_cpp =
"cc1 -E %{traditional|traditional-cpp:-traditional-cpp}";

/* We don't wrap .d files in %W{} since a missing .d file, and
   therefore no dependency entry, confuses make into thinking a .o
   file that happens to exist is up-to-date.  */
static const char *cpp_unique_options =
"%{!Q:-quiet} %{nostdinc*} %{C} %{CC} %{v} %{I*&F*} %{P} %I\
 %{MD:-MD %{!o:%b.d}%{o*:%.d%*}}\
 %{MMD:-MMD %{!o:%b.d}%{o*:%.d%*}}\
 %{M} %{MM} %{MF*} %{MG} %{MP} %{MQ*} %{MT*}\
 %{!E:%{!M:%{!MM:%{!MT:%{!MQ:%{MD|MMD:%{o*:-MQ %*}}}}}}}\
 %{remap} %{g3|ggdb3|gstabs3|gcoff3|gxcoff3|gvms3:-dD}\
 %{!iplugindir*:%{fplugin*:%:find-plugindir()}}\
 %{H} %C %{D*&U*&A*} %{i*} %Z %i\
 %{fmudflap:-D_MUDFLAP -include mf-runtime.h}\
 %{fmudflapth:-D_MUDFLAP -D_MUDFLAPTH -include mf-runtime.h}\
 %{E|M|MM:%W{o*}}";

/* This contains cpp options which are common with cc1_options and are passed
   only when preprocessing only to avoid duplication.  We pass the cc1 spec
   options to the preprocessor so that it the cc1 spec may manipulate
   options used to set target flags.  Those special target flags settings may
   in turn cause preprocessor symbols to be defined specially.  */
static const char *cpp_options =
"%(cpp_unique_options) %1 %{m*} %{std*&ansi&trigraphs} %{W*&pedantic*} %{w}\
 %{f*} %{g*:%{!g0:%{g*} %{!fno-working-directory:-fworking-directory}}} %{O*}\
 %{undef} %{save-temps*:-fpch-preprocess}";

/* This contains cpp options which are not passed when the preprocessor
   output will be used by another program.  */
static const char *cpp_debug_options = "%{d*}";

/* NB: This is shared amongst all front-ends, except for Ada.  */
static const char *cc1_options =
"%{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
 %{!iplugindir*:%{fplugin*:%:find-plugindir()}}\
 %1 %{!Q:-quiet} %{!dumpbase:-dumpbase %B} %{d*} %{m*} %{aux-info*}\
 %{fcompare-debug-second:%:compare-debug-auxbase-opt(%b)} \
 %{!fcompare-debug-second:%{c|S:%{o*:-auxbase-strip %*}%{!o*:-auxbase %b}}}%{!c:%{!S:-auxbase %b}} \
 %{g*} %{O*} %{W*&pedantic*} %{w} %{std*&ansi&trigraphs}\
 %{v:-version} %{pg:-p} %{p} %{f*} %{undef}\
 %{Qn:-fno-ident} %{Qy:} %{-help:--help}\
 %{-target-help:--target-help}\
 %{-version:--version}\
 %{-help=*:--help=%*}\
 %{!fsyntax-only:%{S:%W{o*}%{!o*:-o %b.s}}}\
 %{fsyntax-only:-o %j} %{-param*}\
 %{fmudflap|fmudflapth:-fno-builtin -fno-merge-constants}\
 %{coverage:-fprofile-arcs -ftest-coverage}";

static const char *asm_options =
"%{-target-help:%:print-asm-header()} "
#if HAVE_GNU_AS
/* If GNU AS is used, then convert -w (no warnings), -I, and -v
   to the assembler equivalents.  */
"%{v} %{w:-W} %{I*} "
#endif
"%a %Y %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}";

static const char *invoke_as =
#ifdef AS_NEEDS_DASH_FOR_PIPED_INPUT
"%{!fwpa:\
   %{fcompare-debug=*|fdump-final-insns=*:%:compare-debug-dump-opt()}\
   %{!S:-o %|.s |\n as %(asm_options) %|.s %A }\
  }";
#else
"%{!fwpa:\
   %{fcompare-debug=*|fdump-final-insns=*:%:compare-debug-dump-opt()}\
   %{!S:-o %|.s |\n as %(asm_options) %m.s %A }\
  }";
#endif

/* Some compilers have limits on line lengths, and the multilib_select
   and/or multilib_matches strings can be very long, so we build them at
   run time.  */
static struct obstack multilib_obstack;
static const char *multilib_select;
static const char *multilib_matches;
static const char *multilib_defaults;
static const char *multilib_exclusions;
static const char *multilib_reuse;

/* Check whether a particular argument is a default argument.  */

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "" }
#endif

static const char *const multilib_defaults_raw[] = MULTILIB_DEFAULTS;

#ifndef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS ""
#endif

/* Adding -fopenmp should imply pthreads.  This is particularly important
   for targets that use different start files and suchlike.  */
#ifndef GOMP_SELF_SPECS
#define GOMP_SELF_SPECS "%{fopenmp|ftree-parallelize-loops=*: -pthread}"
#endif

/* Likewise for -fgnu-tm.  */
#ifndef GTM_SELF_SPECS
#define GTM_SELF_SPECS "%{fgnu-tm: -pthread}"
#endif

static const char *const driver_self_specs[] = {
  "%{fdump-final-insns:-fdump-final-insns=.} %<fdump-final-insns",
  DRIVER_SELF_SPECS, CONFIGURE_SPECS, GOMP_SELF_SPECS, GTM_SELF_SPECS
};

#ifndef OPTION_DEFAULT_SPECS
#define OPTION_DEFAULT_SPECS { "", "" }
#endif

struct default_spec
{
  const char *name;
  const char *spec;
};

static const struct default_spec
  option_default_specs[] = { OPTION_DEFAULT_SPECS };

struct user_specs
{
  struct user_specs *next;
  const char *filename;
};

static struct user_specs *user_specs_head, *user_specs_tail;


/* Record the mapping from file suffixes for compilation specs.  */

struct compiler
{
  const char *suffix;		/* Use this compiler for input files
				   whose names end in this suffix.  */

  const char *spec;		/* To use this compiler, run this spec.  */

  const char *cpp_spec;         /* If non-NULL, substitute this spec
				   for `%C', rather than the usual
				   cpp_spec.  */
  const int combinable;          /* If nonzero, compiler can deal with
				    multiple source files at once (IMA).  */
  const int needs_preprocessing; /* If nonzero, source files need to
				    be run through a preprocessor.  */
};

/* Pointer to a vector of `struct compiler' that gives the spec for
   compiling a file, based on its suffix.
   A file that does not end in any of these suffixes will be passed
   unchanged to the loader and nothing else will be done to it.

   An entry containing two 0s is used to terminate the vector.

   If multiple entries match a file, the last matching one is used.  */

static struct compiler *compilers;

/* Number of entries in `compilers', not counting the null terminator.  */

static int n_compilers;

/* The default list of file name suffixes and their compilation specs.  */

static const struct compiler default_compilers[] =
{
  /* Add lists of suffixes of known languages here.  If those languages
     were not present when we built the driver, we will hit these copies
     and be given a more meaningful error than "file not used since
     linking is not done".  */
  {".m",  "#Objective-C", 0, 0, 0}, {".mi",  "#Objective-C", 0, 0, 0},
  {".mm", "#Objective-C++", 0, 0, 0}, {".M", "#Objective-C++", 0, 0, 0},
  {".mii", "#Objective-C++", 0, 0, 0},
  {".cc", "#C++", 0, 0, 0}, {".cxx", "#C++", 0, 0, 0},
  {".cpp", "#C++", 0, 0, 0}, {".cp", "#C++", 0, 0, 0},
  {".c++", "#C++", 0, 0, 0}, {".C", "#C++", 0, 0, 0},
  {".CPP", "#C++", 0, 0, 0}, {".ii", "#C++", 0, 0, 0},
  {".ads", "#Ada", 0, 0, 0}, {".adb", "#Ada", 0, 0, 0},
  {".f", "#Fortran", 0, 0, 0}, {".F", "#Fortran", 0, 0, 0},
  {".for", "#Fortran", 0, 0, 0}, {".FOR", "#Fortran", 0, 0, 0},
  {".ftn", "#Fortran", 0, 0, 0}, {".FTN", "#Fortran", 0, 0, 0},
  {".fpp", "#Fortran", 0, 0, 0}, {".FPP", "#Fortran", 0, 0, 0},
  {".f90", "#Fortran", 0, 0, 0}, {".F90", "#Fortran", 0, 0, 0},
  {".f95", "#Fortran", 0, 0, 0}, {".F95", "#Fortran", 0, 0, 0},
  {".f03", "#Fortran", 0, 0, 0}, {".F03", "#Fortran", 0, 0, 0},
  {".f08", "#Fortran", 0, 0, 0}, {".F08", "#Fortran", 0, 0, 0},
  {".r", "#Ratfor", 0, 0, 0},
  {".p", "#Pascal", 0, 0, 0}, {".pas", "#Pascal", 0, 0, 0},
  {".java", "#Java", 0, 0, 0}, {".class", "#Java", 0, 0, 0},
  {".zip", "#Java", 0, 0, 0}, {".jar", "#Java", 0, 0, 0},
  {".go", "#Go", 0, 1, 0},
  /* Next come the entries for C.  */
  {".c", "@c", 0, 0, 1},
  {"@c",
   /* cc1 has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps is given.  */
     "%{E|M|MM:%(trad_capable_cpp) %(cpp_options) %(cpp_debug_options)}\
      %{!E:%{!M:%{!MM:\
          %{traditional:\
%eGNU C no longer supports -traditional without -E}\
      %{save-temps*|traditional-cpp|no-integrated-cpp:%(trad_capable_cpp) \
	  %(cpp_options) -o %{save-temps*:%b.i} %{!save-temps*:%g.i} \n\
	    cc1 -fpreprocessed %{save-temps*:%b.i} %{!save-temps*:%g.i} \
	  %(cc1_options)}\
      %{!save-temps*:%{!traditional-cpp:%{!no-integrated-cpp:\
	  cc1 %(cpp_unique_options) %(cc1_options)}}}\
      %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 1},
  {"-",
   "%{!E:%e-E or -x required when input is from standard input}\
    %(trad_capable_cpp) %(cpp_options) %(cpp_debug_options)", 0, 0, 0},
  {".h", "@c-header", 0, 0, 0},
  {"@c-header",
   /* cc1 has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps is given.  */
     "%{E|M|MM:%(trad_capable_cpp) %(cpp_options) %(cpp_debug_options)}\
      %{!E:%{!M:%{!MM:\
	  %{save-temps*|traditional-cpp|no-integrated-cpp:%(trad_capable_cpp) \
		%(cpp_options) -o %{save-temps*:%b.i} %{!save-temps*:%g.i} \n\
		    cc1 -fpreprocessed %{save-temps*:%b.i} %{!save-temps*:%g.i} \
			%(cc1_options)\
                        %{!fdump-ada-spec*:-o %g.s %{!o*:--output-pch=%i.gch}\
                        %W{o*:--output-pch=%*}}%V}\
	  %{!save-temps*:%{!traditional-cpp:%{!no-integrated-cpp:\
		cc1 %(cpp_unique_options) %(cc1_options)\
                    %{!fdump-ada-spec*:-o %g.s %{!o*:--output-pch=%i.gch}\
                    %W{o*:--output-pch=%*}}%V}}}}}}", 0, 0, 0},
  {".i", "@cpp-output", 0, 0, 0},
  {"@cpp-output",
   "%{!M:%{!MM:%{!E:cc1 -fpreprocessed %i %(cc1_options) %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 0},
  {".s", "@assembler", 0, 0, 0},
  {"@assembler",
   "%{!M:%{!MM:%{!E:%{!S:as %(asm_debug) %(asm_options) %i %A }}}}", 0, 0, 0},
  {".sx", "@assembler-with-cpp", 0, 0, 0},
  {".S", "@assembler-with-cpp", 0, 0, 0},
  {"@assembler-with-cpp",
#ifdef AS_NEEDS_DASH_FOR_PIPED_INPUT
   "%(trad_capable_cpp) -lang-asm %(cpp_options) -fno-directives-only\
      %{E|M|MM:%(cpp_debug_options)}\
      %{!M:%{!MM:%{!E:%{!S:-o %|.s |\n\
       as %(asm_debug) %(asm_options) %|.s %A }}}}"
#else
   "%(trad_capable_cpp) -lang-asm %(cpp_options) -fno-directives-only\
      %{E|M|MM:%(cpp_debug_options)}\
      %{!M:%{!MM:%{!E:%{!S:-o %|.s |\n\
       as %(asm_debug) %(asm_options) %m.s %A }}}}"
#endif
   , 0, 0, 0},

#include "specs.h"
  /* Mark end of table.  */
  {0, 0, 0, 0, 0}
};

/* Number of elements in default_compilers, not counting the terminator.  */

static const int n_default_compilers = ARRAY_SIZE (default_compilers) - 1;

typedef char *char_p; /* For DEF_VEC_P.  */

/* A vector of options to give to the linker.
   These options are accumulated by %x,
   and substituted into the linker command with %X.  */
static vec<char_p> linker_options;

/* A vector of options to give to the assembler.
   These options are accumulated by -Wa,
   and substituted into the assembler command with %Y.  */
static vec<char_p> assembler_options;

/* A vector of options to give to the preprocessor.
   These options are accumulated by -Wp,
   and substituted into the preprocessor command with %Z.  */
static vec<char_p> preprocessor_options;

static char *
skip_whitespace (char *p)
{
  while (1)
    {
      /* A fully-blank line is a delimiter in the SPEC file and shouldn't
	 be considered whitespace.  */
      if (p[0] == '\n' && p[1] == '\n' && p[2] == '\n')
	return p + 1;
      else if (*p == '\n' || *p == ' ' || *p == '\t')
	p++;
      else if (*p == '#')
	{
	  while (*p != '\n')
	    p++;
	  p++;
	}
      else
	break;
    }

  return p;
}
/* Structures to keep track of prefixes to try when looking for files.  */

struct prefix_list
{
  const char *prefix;	      /* String to prepend to the path.  */
  struct prefix_list *next;   /* Next in linked list.  */
  int require_machine_suffix; /* Don't use without machine_suffix.  */
  /* 2 means try both machine_suffix and just_machine_suffix.  */
  int priority;		      /* Sort key - priority within list.  */
  int os_multilib;	      /* 1 if OS multilib scheme should be used,
				 0 for GCC multilib scheme.  */
};

struct path_prefix
{
  struct prefix_list *plist;  /* List of prefixes to try */
  int max_len;                /* Max length of a prefix in PLIST */
  const char *name;           /* Name of this list (used in config stuff) */
};

/* List of prefixes to try when looking for executables.  */

static struct path_prefix exec_prefixes = { 0, 0, "exec" };

/* List of prefixes to try when looking for startup (crt0) files.  */

static struct path_prefix startfile_prefixes = { 0, 0, "startfile" };

/* List of prefixes to try when looking for include files.  */

static struct path_prefix include_prefixes = { 0, 0, "include" };

/* Suffix to attach to directories searched for commands.
   This looks like `MACHINE/VERSION/'.  */

static const char *machine_suffix = 0;

/* Suffix to attach to directories searched for commands.
   This is just `MACHINE/'.  */

static const char *just_machine_suffix = 0;

/* Adjusted value of GCC_EXEC_PREFIX envvar.  */

static const char *gcc_exec_prefix;

/* Adjusted value of standard_libexec_prefix.  */

static const char *gcc_libexec_prefix;

/* Default prefixes to attach to command names.  */

#ifndef STANDARD_STARTFILE_PREFIX_1
#define STANDARD_STARTFILE_PREFIX_1 "/lib/"
#endif
#ifndef STANDARD_STARTFILE_PREFIX_2
#define STANDARD_STARTFILE_PREFIX_2 "/usr/lib/"
#endif

#ifdef CROSS_DIRECTORY_STRUCTURE  /* Don't use these prefixes for a cross compiler.  */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef MD_STARTFILE_PREFIX_1
#endif

/* If no prefixes defined, use the null string, which will disable them.  */
#ifndef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX ""
#endif
#ifndef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX ""
#endif
#ifndef MD_STARTFILE_PREFIX_1
#define MD_STARTFILE_PREFIX_1 ""
#endif

/* These directories are locations set at configure-time based on the
   --prefix option provided to configure.  Their initializers are
   defined in Makefile.in.  These paths are not *directly* used when
   gcc_exec_prefix is set because, in that case, we know where the
   compiler has been installed, and use paths relative to that
   location instead.  */
static const char *const standard_exec_prefix = STANDARD_EXEC_PREFIX;
static const char *const standard_libexec_prefix = STANDARD_LIBEXEC_PREFIX;
static const char *const standard_bindir_prefix = STANDARD_BINDIR_PREFIX;
static const char *const standard_startfile_prefix = STANDARD_STARTFILE_PREFIX;

/* For native compilers, these are well-known paths containing
   components that may be provided by the system.  For cross
   compilers, these paths are not used.  */
static const char *md_exec_prefix = MD_EXEC_PREFIX;
static const char *md_startfile_prefix = MD_STARTFILE_PREFIX;
static const char *md_startfile_prefix_1 = MD_STARTFILE_PREFIX_1;
static const char *const standard_startfile_prefix_1
  = STANDARD_STARTFILE_PREFIX_1;
static const char *const standard_startfile_prefix_2
  = STANDARD_STARTFILE_PREFIX_2;

/* A relative path to be used in finding the location of tools
   relative to the driver.  */
static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;

/* Subdirectory to use for locating libraries.  Set by
   set_multilib_dir based on the compilation options.  */

static const char *multilib_dir;

/* Subdirectory to use for locating libraries in OS conventions.  Set by
   set_multilib_dir based on the compilation options.  */

static const char *multilib_os_dir;

/* Subdirectory to use for locating libraries in multiarch conventions.  Set by
   set_multilib_dir based on the compilation options.  */

static const char *multiarch_dir;

/* Structure to keep track of the specs that have been defined so far.
   These are accessed using %(specname) in a compiler or link
   spec.  */

struct spec_list
{
				/* The following 2 fields must be first */
				/* to allow EXTRA_SPECS to be initialized */
  const char *name;		/* name of the spec.  */
  const char *ptr;		/* available ptr if no static pointer */

				/* The following fields are not initialized */
				/* by EXTRA_SPECS */
  const char **ptr_spec;	/* pointer to the spec itself.  */
  struct spec_list *next;	/* Next spec in linked list.  */
  int name_len;			/* length of the name */
  bool user_p;			/* whether string come from file spec.  */
  bool alloc_p;			/* whether string was allocated */
};

#define INIT_STATIC_SPEC(NAME,PTR) \
  { NAME, NULL, PTR, (struct spec_list *) 0, sizeof (NAME) - 1, false, false }

/* List of statically defined specs.  */
static struct spec_list static_specs[] =
{
  INIT_STATIC_SPEC ("asm",			&asm_spec),
  INIT_STATIC_SPEC ("asm_debug",		&asm_debug),
  INIT_STATIC_SPEC ("asm_final",		&asm_final_spec),
  INIT_STATIC_SPEC ("asm_options",		&asm_options),
  INIT_STATIC_SPEC ("invoke_as",		&invoke_as),
  INIT_STATIC_SPEC ("cpp",			&cpp_spec),
  INIT_STATIC_SPEC ("cpp_options",		&cpp_options),
  INIT_STATIC_SPEC ("cpp_debug_options",	&cpp_debug_options),
  INIT_STATIC_SPEC ("cpp_unique_options",	&cpp_unique_options),
  INIT_STATIC_SPEC ("trad_capable_cpp",		&trad_capable_cpp),
  INIT_STATIC_SPEC ("cc1",			&cc1_spec),
  INIT_STATIC_SPEC ("cc1_options",		&cc1_options),
  INIT_STATIC_SPEC ("cc1plus",			&cc1plus_spec),
  INIT_STATIC_SPEC ("link_gcc_c_sequence",	&link_gcc_c_sequence_spec),
  INIT_STATIC_SPEC ("link_ssp",			&link_ssp_spec),
  INIT_STATIC_SPEC ("endfile",			&endfile_spec),
  INIT_STATIC_SPEC ("link",			&link_spec),
  INIT_STATIC_SPEC ("lib",			&lib_spec),
  INIT_STATIC_SPEC ("mfwrap",			&mfwrap_spec),
  INIT_STATIC_SPEC ("mflib",			&mflib_spec),
  INIT_STATIC_SPEC ("link_gomp",		&link_gomp_spec),
  INIT_STATIC_SPEC ("libgcc",			&libgcc_spec),
  INIT_STATIC_SPEC ("startfile",		&startfile_spec),
  INIT_STATIC_SPEC ("cross_compile",		&cross_compile),
  INIT_STATIC_SPEC ("version",			&compiler_version),
  INIT_STATIC_SPEC ("multilib",			&multilib_select),
  INIT_STATIC_SPEC ("multilib_defaults",	&multilib_defaults),
  INIT_STATIC_SPEC ("multilib_extra",		&multilib_extra),
  INIT_STATIC_SPEC ("multilib_matches",		&multilib_matches),
  INIT_STATIC_SPEC ("multilib_exclusions",	&multilib_exclusions),
  INIT_STATIC_SPEC ("multilib_options",		&multilib_options),
  INIT_STATIC_SPEC ("multilib_reuse",		&multilib_reuse),
  INIT_STATIC_SPEC ("linker",			&linker_name_spec),
  INIT_STATIC_SPEC ("linker_plugin_file",	&linker_plugin_file_spec),
  INIT_STATIC_SPEC ("lto_wrapper",		&lto_wrapper_spec),
  INIT_STATIC_SPEC ("lto_gcc",			&lto_gcc_spec),
  INIT_STATIC_SPEC ("link_libgcc",		&link_libgcc_spec),
  INIT_STATIC_SPEC ("md_exec_prefix",		&md_exec_prefix),
  INIT_STATIC_SPEC ("md_startfile_prefix",	&md_startfile_prefix),
  INIT_STATIC_SPEC ("md_startfile_prefix_1",	&md_startfile_prefix_1),
  INIT_STATIC_SPEC ("startfile_prefix_spec",	&startfile_prefix_spec),
  INIT_STATIC_SPEC ("sysroot_spec",             &sysroot_spec),
  INIT_STATIC_SPEC ("sysroot_suffix_spec",	&sysroot_suffix_spec),
  INIT_STATIC_SPEC ("sysroot_hdrs_suffix_spec",	&sysroot_hdrs_suffix_spec),
  INIT_STATIC_SPEC ("self_spec",		&self_spec),
};

#ifdef EXTRA_SPECS		/* additional specs needed */
/* Structure to keep track of just the first two args of a spec_list.
   That is all that the EXTRA_SPECS macro gives us.  */
struct spec_list_1
{
  const char *const name;
  const char *const ptr;
};

static const struct spec_list_1 extra_specs_1[] = { EXTRA_SPECS };
static struct spec_list *extra_specs = (struct spec_list *) 0;
#endif

/* List of dynamically allocates specs that have been defined so far.  */

static struct spec_list *specs = (struct spec_list *) 0;

/* List of static spec functions.  */

static const struct spec_function static_spec_functions[] =
{
  { "getenv",                   getenv_spec_function },
  { "if-exists",		if_exists_spec_function },
  { "if-exists-else",		if_exists_else_spec_function },
  { "replace-outfile",		replace_outfile_spec_function },
  { "remove-outfile",		remove_outfile_spec_function },
  { "version-compare",		version_compare_spec_function },
  { "include",			include_spec_function },
  { "find-file",		find_file_spec_function },
  { "find-plugindir",		find_plugindir_spec_function },
  { "print-asm-header",		print_asm_header_spec_function },
  { "compare-debug-dump-opt",	compare_debug_dump_opt_spec_function },
  { "compare-debug-self-opt",	compare_debug_self_opt_spec_function },
  { "compare-debug-auxbase-opt", compare_debug_auxbase_opt_spec_function },
  { "pass-through-libs",	pass_through_libs_spec_func },
  { "replace-extension",	replace_extension_spec_func },
#ifdef EXTRA_SPEC_FUNCTIONS
  EXTRA_SPEC_FUNCTIONS
#endif
  { 0, 0 }
};

static int processing_spec_function;

/* Add appropriate libgcc specs to OBSTACK, taking into account
   various permutations of -shared-libgcc, -shared, and such.  */

#if defined(ENABLE_SHARED_LIBGCC) && !defined(REAL_LIBGCC_SPEC)

#ifndef USE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 0
#endif

static void
init_gcc_specs (struct obstack *obstack, const char *shared_name,
		const char *static_name, const char *eh_name)
{
  char *buf;

  buf = concat ("%{static|static-libgcc:", static_name, " ", eh_name, "}"
		"%{!static:%{!static-libgcc:"
#if USE_LD_AS_NEEDED
		"%{!shared-libgcc:",
		static_name, " " LD_AS_NEEDED_OPTION " ",
		shared_name, " " LD_NO_AS_NEEDED_OPTION
		"}"
		"%{shared-libgcc:",
		shared_name, "%{!shared: ", static_name, "}"
		"}"
#else
		"%{!shared:"
		"%{!shared-libgcc:", static_name, " ", eh_name, "}"
		"%{shared-libgcc:", shared_name, " ", static_name, "}"
		"}"
#ifdef LINK_EH_SPEC
		"%{shared:"
		"%{shared-libgcc:", shared_name, "}"
		"%{!shared-libgcc:", static_name, "}"
		"}"
#else
		"%{shared:", shared_name, "}"
#endif
#endif
		"}}", NULL);

  obstack_grow (obstack, buf, strlen (buf));
  free (buf);
}
#endif /* ENABLE_SHARED_LIBGCC */

/* Initialize the specs lookup routines.  */

static void
init_spec (void)
{
  struct spec_list *next = (struct spec_list *) 0;
  struct spec_list *sl   = (struct spec_list *) 0;
  int i;

  if (specs)
    return;			/* Already initialized.  */

  if (verbose_flag)
    fnotice (stderr, "Using built-in specs.\n");

#ifdef EXTRA_SPECS
  extra_specs = XCNEWVEC (struct spec_list, ARRAY_SIZE (extra_specs_1));

  for (i = ARRAY_SIZE (extra_specs_1) - 1; i >= 0; i--)
    {
      sl = &extra_specs[i];
      sl->name = extra_specs_1[i].name;
      sl->ptr = extra_specs_1[i].ptr;
      sl->next = next;
      sl->name_len = strlen (sl->name);
      sl->ptr_spec = &sl->ptr;
      next = sl;
    }
#endif

  for (i = ARRAY_SIZE (static_specs) - 1; i >= 0; i--)
    {
      sl = &static_specs[i];
      sl->next = next;
      next = sl;
    }

#if defined(ENABLE_SHARED_LIBGCC) && !defined(REAL_LIBGCC_SPEC)
  /* ??? If neither -shared-libgcc nor --static-libgcc was
     seen, then we should be making an educated guess.  Some proposed
     heuristics for ELF include:

	(1) If "-Wl,--export-dynamic", then it's a fair bet that the
	    program will be doing dynamic loading, which will likely
	    need the shared libgcc.

	(2) If "-ldl", then it's also a fair bet that we're doing
	    dynamic loading.

	(3) For each ET_DYN we're linking against (either through -lfoo
	    or /some/path/foo.so), check to see whether it or one of
	    its dependencies depends on a shared libgcc.

	(4) If "-shared"

	    If the runtime is fixed to look for program headers instead
	    of calling __register_frame_info at all, for each object,
	    use the shared libgcc if any EH symbol referenced.

	    If crtstuff is fixed to not invoke __register_frame_info
	    automatically, for each object, use the shared libgcc if
	    any non-empty unwind section found.

     Doing any of this probably requires invoking an external program to
     do the actual object file scanning.  */
  {
    const char *p = libgcc_spec;
    int in_sep = 1;

    /* Transform the extant libgcc_spec into one that uses the shared libgcc
       when given the proper command line arguments.  */
    while (*p)
      {
	if (in_sep && *p == '-' && strncmp (p, "-lgcc", 5) == 0)
	  {
	    init_gcc_specs (&obstack,
			    "-lgcc_s"
#ifdef USE_LIBUNWIND_EXCEPTIONS
			    " -lunwind"
#endif
			    ,
			    "-lgcc",
			    "-lgcc_eh"
#ifdef USE_LIBUNWIND_EXCEPTIONS
# ifdef HAVE_LD_STATIC_DYNAMIC
			    " %{!static:" LD_STATIC_OPTION "} -lunwind"
			    " %{!static:" LD_DYNAMIC_OPTION "}"
# else
			    " -lunwind"
# endif
#endif
			    );

	    p += 5;
	    in_sep = 0;
	  }
	else if (in_sep && *p == 'l' && strncmp (p, "libgcc.a%s", 10) == 0)
	  {
	    /* Ug.  We don't know shared library extensions.  Hope that
	       systems that use this form don't do shared libraries.  */
	    init_gcc_specs (&obstack,
			    "-lgcc_s",
			    "libgcc.a%s",
			    "libgcc_eh.a%s"
#ifdef USE_LIBUNWIND_EXCEPTIONS
			    " -lunwind"
#endif
			    );
	    p += 10;
	    in_sep = 0;
	  }
	else
	  {
	    obstack_1grow (&obstack, *p);
	    in_sep = (*p == ' ');
	    p += 1;
	  }
      }

    obstack_1grow (&obstack, '\0');
    libgcc_spec = XOBFINISH (&obstack, const char *);
  }
#endif
#ifdef USE_AS_TRADITIONAL_FORMAT
  /* Prepend "--traditional-format" to whatever asm_spec we had before.  */
  {
    static const char tf[] = "--traditional-format ";
    obstack_grow (&obstack, tf, sizeof(tf) - 1);
    obstack_grow0 (&obstack, asm_spec, strlen (asm_spec));
    asm_spec = XOBFINISH (&obstack, const char *);
  }
#endif

#if defined LINK_EH_SPEC || defined LINK_BUILDID_SPEC || \
    defined LINKER_HASH_STYLE
# ifdef LINK_BUILDID_SPEC
  /* Prepend LINK_BUILDID_SPEC to whatever link_spec we had before.  */
  obstack_grow (&obstack, LINK_BUILDID_SPEC, sizeof(LINK_BUILDID_SPEC) - 1);
# endif
# ifdef LINK_EH_SPEC
  /* Prepend LINK_EH_SPEC to whatever link_spec we had before.  */
  obstack_grow (&obstack, LINK_EH_SPEC, sizeof(LINK_EH_SPEC) - 1);
# endif
# ifdef LINKER_HASH_STYLE
  /* Prepend --hash-style=LINKER_HASH_STYLE to whatever link_spec we had
     before.  */
  {
    static const char hash_style[] = "--hash-style=";
    obstack_grow (&obstack, hash_style, sizeof(hash_style) - 1);
    obstack_grow (&obstack, LINKER_HASH_STYLE, sizeof(LINKER_HASH_STYLE) - 1);
    obstack_1grow (&obstack, ' ');
  }
# endif
  obstack_grow0 (&obstack, link_spec, strlen (link_spec));
  link_spec = XOBFINISH (&obstack, const char *);
#endif

  specs = sl;
}

/* Change the value of spec NAME to SPEC.  If SPEC is empty, then the spec is
   removed; If the spec starts with a + then SPEC is added to the end of the
   current spec.  */

static void
set_spec (const char *name, const char *spec, bool user_p)
{
  struct spec_list *sl;
  const char *old_spec;
  int name_len = strlen (name);
  int i;

  /* If this is the first call, initialize the statically allocated specs.  */
  if (!specs)
    {
      struct spec_list *next = (struct spec_list *) 0;
      for (i = ARRAY_SIZE (static_specs) - 1; i >= 0; i--)
	{
	  sl = &static_specs[i];
	  sl->next = next;
	  next = sl;
	}
      specs = sl;
    }

  /* See if the spec already exists.  */
  for (sl = specs; sl; sl = sl->next)
    if (name_len == sl->name_len && !strcmp (sl->name, name))
      break;

  if (!sl)
    {
      /* Not found - make it.  */
      sl = XNEW (struct spec_list);
      sl->name = xstrdup (name);
      sl->name_len = name_len;
      sl->ptr_spec = &sl->ptr;
      sl->alloc_p = 0;
      *(sl->ptr_spec) = "";
      sl->next = specs;
      specs = sl;
    }

  old_spec = *(sl->ptr_spec);
  *(sl->ptr_spec) = ((spec[0] == '+' && ISSPACE ((unsigned char)spec[1]))
		     ? concat (old_spec, spec + 1, NULL)
		     : xstrdup (spec));

#ifdef DEBUG_SPECS
  if (verbose_flag)
    fnotice (stderr, "Setting spec %s to '%s'\n\n", name, *(sl->ptr_spec));
#endif

  /* Free the old spec.  */
  if (old_spec && sl->alloc_p)
    free (CONST_CAST(char *, old_spec));

  sl->user_p = user_p;
  sl->alloc_p = true;
}

/* Accumulate a command (program name and args), and run it.  */

typedef const char *const_char_p; /* For DEF_VEC_P.  */

/* Vector of pointers to arguments in the current line of specifications.  */

static vec<const_char_p> argbuf;

/* Position in the argbuf vector containing the name of the output file
   (the value associated with the "-o" flag).  */

static int have_o_argbuf_index = 0;

/* Were the options -c, -S or -E passed.  */
static int have_c = 0;

/* Was the option -o passed.  */
static int have_o = 0;

/* This is the list of suffixes and codes (%g/%u/%U/%j) and the associated
   temp file.  If the HOST_BIT_BUCKET is used for %j, no entry is made for
   it here.  */

static struct temp_name {
  const char *suffix;	/* suffix associated with the code.  */
  int length;		/* strlen (suffix).  */
  int unique;		/* Indicates whether %g or %u/%U was used.  */
  const char *filename;	/* associated filename.  */
  int filename_length;	/* strlen (filename).  */
  struct temp_name *next;
} *temp_names;

/* Number of commands executed so far.  */

static int execution_count;

/* Number of commands that exited with a signal.  */

static int signal_count;

/* Allocate the argument vector.  */

static void
alloc_args (void)
{
  argbuf.create (10);
}

/* Clear out the vector of arguments (after a command is executed).  */

static void
clear_args (void)
{
  argbuf.truncate (0);
}

/* Add one argument to the vector at the end.
   This is done when a space is seen or at the end of the line.
   If DELETE_ALWAYS is nonzero, the arg is a filename
    and the file should be deleted eventually.
   If DELETE_FAILURE is nonzero, the arg is a filename
    and the file should be deleted if this compilation fails.  */

static void
store_arg (const char *arg, int delete_always, int delete_failure)
{
  argbuf.safe_push (arg);

  if (strcmp (arg, "-o") == 0)
    have_o_argbuf_index = argbuf.length ();
  if (delete_always || delete_failure)
    {
      const char *p;
      /* If the temporary file we should delete is specified as
	 part of a joined argument extract the filename.  */
      if (arg[0] == '-'
	  && (p = strrchr (arg, '=')))
	arg = p + 1;
      record_temp_file (arg, delete_always, delete_failure);
    }
}

/* Load specs from a file name named FILENAME, replacing occurrences of
   various different types of line-endings, \r\n, \n\r and just \r, with
   a single \n.  */

static char *
load_specs (const char *filename)
{
  int desc;
  int readlen;
  struct stat statbuf;
  char *buffer;
  char *buffer_p;
  char *specs;
  char *specs_p;

  if (verbose_flag)
    fnotice (stderr, "Reading specs from %s\n", filename);

  /* Open and stat the file.  */
  desc = open (filename, O_RDONLY, 0);
  if (desc < 0)
    pfatal_with_name (filename);
  if (stat (filename, &statbuf) < 0)
    pfatal_with_name (filename);

  /* Read contents of file into BUFFER.  */
  buffer = XNEWVEC (char, statbuf.st_size + 1);
  readlen = read (desc, buffer, (unsigned) statbuf.st_size);
  if (readlen < 0)
    pfatal_with_name (filename);
  buffer[readlen] = 0;
  close (desc);

  specs = XNEWVEC (char, readlen + 1);
  specs_p = specs;
  for (buffer_p = buffer; buffer_p && *buffer_p; buffer_p++)
    {
      int skip = 0;
      char c = *buffer_p;
      if (c == '\r')
	{
	  if (buffer_p > buffer && *(buffer_p - 1) == '\n')	/* \n\r */
	    skip = 1;
	  else if (*(buffer_p + 1) == '\n')			/* \r\n */
	    skip = 1;
	  else							/* \r */
	    c = '\n';
	}
      if (! skip)
	*specs_p++ = c;
    }
  *specs_p = '\0';

  free (buffer);
  return (specs);
}

/* Read compilation specs from a file named FILENAME,
   replacing the default ones.

   A suffix which starts with `*' is a definition for
   one of the machine-specific sub-specs.  The "suffix" should be
   *asm, *cc1, *cpp, *link, *startfile, etc.
   The corresponding spec is stored in asm_spec, etc.,
   rather than in the `compilers' vector.

   Anything invalid in the file is a fatal error.  */

static void
read_specs (const char *filename, bool main_p, bool user_p)
{
  char *buffer;
  char *p;

  buffer = load_specs (filename);

  /* Scan BUFFER for specs, putting them in the vector.  */
  p = buffer;
  while (1)
    {
      char *suffix;
      char *spec;
      char *in, *out, *p1, *p2, *p3;

      /* Advance P in BUFFER to the next nonblank nocomment line.  */
      p = skip_whitespace (p);
      if (*p == 0)
	break;

      /* Is this a special command that starts with '%'? */
      /* Don't allow this for the main specs file, since it would
	 encourage people to overwrite it.  */
      if (*p == '%' && !main_p)
	{
	  p1 = p;
	  while (*p && *p != '\n')
	    p++;

	  /* Skip '\n'.  */
	  p++;

	  if (!strncmp (p1, "%include", sizeof ("%include") - 1)
	      && (p1[sizeof "%include" - 1] == ' '
		  || p1[sizeof "%include" - 1] == '\t'))
	    {
	      char *new_filename;

	      p1 += sizeof ("%include");
	      while (*p1 == ' ' || *p1 == '\t')
		p1++;

	      if (*p1++ != '<' || p[-2] != '>')
		fatal_error ("specs %%include syntax malformed after "
			     "%ld characters",
			     (long) (p1 - buffer + 1));

	      p[-2] = '\0';
	      new_filename = find_a_file (&startfile_prefixes, p1, R_OK, true);
	      read_specs (new_filename ? new_filename : p1, false, user_p);
	      continue;
	    }
	  else if (!strncmp (p1, "%include_noerr", sizeof "%include_noerr" - 1)
		   && (p1[sizeof "%include_noerr" - 1] == ' '
		       || p1[sizeof "%include_noerr" - 1] == '\t'))
	    {
	      char *new_filename;

	      p1 += sizeof "%include_noerr";
	      while (*p1 == ' ' || *p1 == '\t')
		p1++;

	      if (*p1++ != '<' || p[-2] != '>')
		fatal_error ("specs %%include syntax malformed after "
			     "%ld characters",
			     (long) (p1 - buffer + 1));

	      p[-2] = '\0';
	      new_filename = find_a_file (&startfile_prefixes, p1, R_OK, true);
	      if (new_filename)
		read_specs (new_filename, false, user_p);
	      else if (verbose_flag)
		fnotice (stderr, "could not find specs file %s\n", p1);
	      continue;
	    }
	  else if (!strncmp (p1, "%rename", sizeof "%rename" - 1)
		   && (p1[sizeof "%rename" - 1] == ' '
		       || p1[sizeof "%rename" - 1] == '\t'))
	    {
	      int name_len;
	      struct spec_list *sl;
	      struct spec_list *newsl;

	      /* Get original name.  */
	      p1 += sizeof "%rename";
	      while (*p1 == ' ' || *p1 == '\t')
		p1++;

	      if (! ISALPHA ((unsigned char) *p1))
		fatal_error ("specs %%rename syntax malformed after "
			     "%ld characters",
			     (long) (p1 - buffer));

	      p2 = p1;
	      while (*p2 && !ISSPACE ((unsigned char) *p2))
		p2++;

	      if (*p2 != ' ' && *p2 != '\t')
		fatal_error ("specs %%rename syntax malformed after "
			     "%ld characters",
			     (long) (p2 - buffer));

	      name_len = p2 - p1;
	      *p2++ = '\0';
	      while (*p2 == ' ' || *p2 == '\t')
		p2++;

	      if (! ISALPHA ((unsigned char) *p2))
		fatal_error ("specs %%rename syntax malformed after "
			     "%ld characters",
			     (long) (p2 - buffer));

	      /* Get new spec name.  */
	      p3 = p2;
	      while (*p3 && !ISSPACE ((unsigned char) *p3))
		p3++;

	      if (p3 != p - 1)
		fatal_error ("specs %%rename syntax malformed after "
			     "%ld characters",
			     (long) (p3 - buffer));
	      *p3 = '\0';

	      for (sl = specs; sl; sl = sl->next)
		if (name_len == sl->name_len && !strcmp (sl->name, p1))
		  break;

	      if (!sl)
		fatal_error ("specs %s spec was not found to be renamed", p1);

	      if (strcmp (p1, p2) == 0)
		continue;

	      for (newsl = specs; newsl; newsl = newsl->next)
		if (strcmp (newsl->name, p2) == 0)
		  fatal_error ("%s: attempt to rename spec %qs to "
			       "already defined spec %qs",
		    filename, p1, p2);

	      if (verbose_flag)
		{
		  fnotice (stderr, "rename spec %s to %s\n", p1, p2);
#ifdef DEBUG_SPECS
		  fnotice (stderr, "spec is '%s'\n\n", *(sl->ptr_spec));
#endif
		}

	      set_spec (p2, *(sl->ptr_spec), user_p);
	      if (sl->alloc_p)
		free (CONST_CAST (char *, *(sl->ptr_spec)));

	      *(sl->ptr_spec) = "";
	      sl->alloc_p = 0;
	      continue;
	    }
	  else
	    fatal_error ("specs unknown %% command after %ld characters",
			 (long) (p1 - buffer));
	}

      /* Find the colon that should end the suffix.  */
      p1 = p;
      while (*p1 && *p1 != ':' && *p1 != '\n')
	p1++;

      /* The colon shouldn't be missing.  */
      if (*p1 != ':')
	fatal_error ("specs file malformed after %ld characters",
		     (long) (p1 - buffer));

      /* Skip back over trailing whitespace.  */
      p2 = p1;
      while (p2 > buffer && (p2[-1] == ' ' || p2[-1] == '\t'))
	p2--;

      /* Copy the suffix to a string.  */
      suffix = save_string (p, p2 - p);
      /* Find the next line.  */
      p = skip_whitespace (p1 + 1);
      if (p[1] == 0)
	fatal_error ("specs file malformed after %ld characters",
		     (long) (p - buffer));

      p1 = p;
      /* Find next blank line or end of string.  */
      while (*p1 && !(*p1 == '\n' && (p1[1] == '\n' || p1[1] == '\0')))
	p1++;

      /* Specs end at the blank line and do not include the newline.  */
      spec = save_string (p, p1 - p);
      p = p1;

      /* Delete backslash-newline sequences from the spec.  */
      in = spec;
      out = spec;
      while (*in != 0)
	{
	  if (in[0] == '\\' && in[1] == '\n')
	    in += 2;
	  else if (in[0] == '#')
	    while (*in && *in != '\n')
	      in++;

	  else
	    *out++ = *in++;
	}
      *out = 0;

      if (suffix[0] == '*')
	{
	  if (! strcmp (suffix, "*link_command"))
	    link_command_spec = spec;
	  else
	    set_spec (suffix + 1, spec, user_p);
	}
      else
	{
	  /* Add this pair to the vector.  */
	  compilers
	    = XRESIZEVEC (struct compiler, compilers, n_compilers + 2);

	  compilers[n_compilers].suffix = suffix;
	  compilers[n_compilers].spec = spec;
	  n_compilers++;
	  memset (&compilers[n_compilers], 0, sizeof compilers[n_compilers]);
	}

      if (*suffix == 0)
	link_command_spec = spec;
    }

  if (link_command_spec == 0)
    fatal_error ("spec file has no spec for linking");
}

/* Record the names of temporary files we tell compilers to write,
   and delete them at the end of the run.  */

/* This is the common prefix we use to make temp file names.
   It is chosen once for each run of this program.
   It is substituted into a spec by %g or %j.
   Thus, all temp file names contain this prefix.
   In practice, all temp file names start with this prefix.

   This prefix comes from the envvar TMPDIR if it is defined;
   otherwise, from the P_tmpdir macro if that is defined;
   otherwise, in /usr/tmp or /tmp;
   or finally the current directory if all else fails.  */

static const char *temp_filename;

/* Length of the prefix.  */

static int temp_filename_length;

/* Define the list of temporary files to delete.  */

struct temp_file
{
  const char *name;
  struct temp_file *next;
};

/* Queue of files to delete on success or failure of compilation.  */
static struct temp_file *always_delete_queue;
/* Queue of files to delete on failure of compilation.  */
static struct temp_file *failure_delete_queue;

/* Record FILENAME as a file to be deleted automatically.
   ALWAYS_DELETE nonzero means delete it if all compilation succeeds;
   otherwise delete it in any case.
   FAIL_DELETE nonzero means delete it if a compilation step fails;
   otherwise delete it in any case.  */

void
record_temp_file (const char *filename, int always_delete, int fail_delete)
{
  char *const name = xstrdup (filename);

  if (always_delete)
    {
      struct temp_file *temp;
      for (temp = always_delete_queue; temp; temp = temp->next)
	if (! filename_cmp (name, temp->name))
	  goto already1;

      temp = XNEW (struct temp_file);
      temp->next = always_delete_queue;
      temp->name = name;
      always_delete_queue = temp;

    already1:;
    }

  if (fail_delete)
    {
      struct temp_file *temp;
      for (temp = failure_delete_queue; temp; temp = temp->next)
	if (! filename_cmp (name, temp->name))
	  {
	    free (name);
	    goto already2;
	  }

      temp = XNEW (struct temp_file);
      temp->next = failure_delete_queue;
      temp->name = name;
      failure_delete_queue = temp;

    already2:;
    }
}

/* Delete all the temporary files whose names we previously recorded.  */

#ifndef DELETE_IF_ORDINARY
#define DELETE_IF_ORDINARY(NAME,ST,VERBOSE_FLAG)        \
do                                                      \
  {                                                     \
    if (stat (NAME, &ST) >= 0 && S_ISREG (ST.st_mode))  \
      if (unlink (NAME) < 0)                            \
	if (VERBOSE_FLAG)                               \
	  perror_with_name (NAME);                      \
  } while (0)
#endif

static void
delete_if_ordinary (const char *name)
{
  struct stat st;
#ifdef DEBUG
  int i, c;

  printf ("Delete %s? (y or n) ", name);
  fflush (stdout);
  i = getchar ();
  if (i != '\n')
    while ((c = getchar ()) != '\n' && c != EOF)
      ;

  if (i == 'y' || i == 'Y')
#endif /* DEBUG */
  DELETE_IF_ORDINARY (name, st, verbose_flag);
}

static void
delete_temp_files (void)
{
  struct temp_file *temp;

  for (temp = always_delete_queue; temp; temp = temp->next)
    delete_if_ordinary (temp->name);
  always_delete_queue = 0;
}

/* Delete all the files to be deleted on error.  */

static void
delete_failure_queue (void)
{
  struct temp_file *temp;

  for (temp = failure_delete_queue; temp; temp = temp->next)
    delete_if_ordinary (temp->name);
}

static void
clear_failure_queue (void)
{
  failure_delete_queue = 0;
}

/* Call CALLBACK for each path in PATHS, breaking out early if CALLBACK
   returns non-NULL.
   If DO_MULTI is true iterate over the paths twice, first with multilib
   suffix then without, otherwise iterate over the paths once without
   adding a multilib suffix.  When DO_MULTI is true, some attempt is made
   to avoid visiting the same path twice, but we could do better.  For
   instance, /usr/lib/../lib is considered different from /usr/lib.
   At least EXTRA_SPACE chars past the end of the path passed to
   CALLBACK are available for use by the callback.
   CALLBACK_INFO allows extra parameters to be passed to CALLBACK.

   Returns the value returned by CALLBACK.  */

static void *
for_each_path (const struct path_prefix *paths,
	       bool do_multi,
	       size_t extra_space,
	       void *(*callback) (char *, void *),
	       void *callback_info)
{
  struct prefix_list *pl;
  const char *multi_dir = NULL;
  const char *multi_os_dir = NULL;
  const char *multiarch_suffix = NULL;
  const char *multi_suffix;
  const char *just_multi_suffix;
  char *path = NULL;
  void *ret = NULL;
  bool skip_multi_dir = false;
  bool skip_multi_os_dir = false;

  multi_suffix = machine_suffix;
  just_multi_suffix = just_machine_suffix;
  if (do_multi && multilib_dir && strcmp (multilib_dir, ".") != 0)
    {
      multi_dir = concat (multilib_dir, dir_separator_str, NULL);
      multi_suffix = concat (multi_suffix, multi_dir, NULL);
      just_multi_suffix = concat (just_multi_suffix, multi_dir, NULL);
    }
  if (do_multi && multilib_os_dir && strcmp (multilib_os_dir, ".") != 0)
    multi_os_dir = concat (multilib_os_dir, dir_separator_str, NULL);
  if (multiarch_dir)
    multiarch_suffix = concat (multiarch_dir, dir_separator_str, NULL);

  while (1)
    {
      size_t multi_dir_len = 0;
      size_t multi_os_dir_len = 0;
      size_t multiarch_len = 0;
      size_t suffix_len;
      size_t just_suffix_len;
      size_t len;

      if (multi_dir)
	multi_dir_len = strlen (multi_dir);
      if (multi_os_dir)
	multi_os_dir_len = strlen (multi_os_dir);
      if (multiarch_suffix)
	multiarch_len = strlen (multiarch_suffix);
      suffix_len = strlen (multi_suffix);
      just_suffix_len = strlen (just_multi_suffix);

      if (path == NULL)
	{
	  len = paths->max_len + extra_space + 1;
	  len += MAX (MAX (suffix_len, multi_os_dir_len), multiarch_len);
	  path = XNEWVEC (char, len);
	}

      for (pl = paths->plist; pl != 0; pl = pl->next)
	{
	  len = strlen (pl->prefix);
	  memcpy (path, pl->prefix, len);

	  /* Look first in MACHINE/VERSION subdirectory.  */
	  if (!skip_multi_dir)
	    {
	      memcpy (path + len, multi_suffix, suffix_len + 1);
	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }

	  /* Some paths are tried with just the machine (ie. target)
	     subdir.  This is used for finding as, ld, etc.  */
	  if (!skip_multi_dir
	      && pl->require_machine_suffix == 2)
	    {
	      memcpy (path + len, just_multi_suffix, just_suffix_len + 1);
	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }

	  /* Now try the multiarch path.  */
	  if (!skip_multi_dir
	      && !pl->require_machine_suffix && multiarch_dir)
	    {
	      memcpy (path + len, multiarch_suffix, multiarch_len + 1);
	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }

	  /* Now try the base path.  */
	  if (!pl->require_machine_suffix
	      && !(pl->os_multilib ? skip_multi_os_dir : skip_multi_dir))
	    {
	      const char *this_multi;
	      size_t this_multi_len;

	      if (pl->os_multilib)
		{
		  this_multi = multi_os_dir;
		  this_multi_len = multi_os_dir_len;
		}
	      else
		{
		  this_multi = multi_dir;
		  this_multi_len = multi_dir_len;
		}

	      if (this_multi_len)
		memcpy (path + len, this_multi, this_multi_len + 1);
	      else
		path[len] = '\0';

	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }
	}
      if (pl)
	break;

      if (multi_dir == NULL && multi_os_dir == NULL)
	break;

      /* Run through the paths again, this time without multilibs.
	 Don't repeat any we have already seen.  */
      if (multi_dir)
	{
	  free (CONST_CAST (char *, multi_dir));
	  multi_dir = NULL;
	  free (CONST_CAST (char *, multi_suffix));
	  multi_suffix = machine_suffix;
	  free (CONST_CAST (char *, just_multi_suffix));
	  just_multi_suffix = just_machine_suffix;
	}
      else
	skip_multi_dir = true;
      if (multi_os_dir)
	{
	  free (CONST_CAST (char *, multi_os_dir));
	  multi_os_dir = NULL;
	}
      else
	skip_multi_os_dir = true;
    }

  if (multi_dir)
    {
      free (CONST_CAST (char *, multi_dir));
      free (CONST_CAST (char *, multi_suffix));
      free (CONST_CAST (char *, just_multi_suffix));
    }
  if (multi_os_dir)
    free (CONST_CAST (char *, multi_os_dir));
  if (ret != path)
    free (path);
  return ret;
}

/* Callback for build_search_list.  Adds path to obstack being built.  */

struct add_to_obstack_info {
  struct obstack *ob;
  bool check_dir;
  bool first_time;
};

static void *
add_to_obstack (char *path, void *data)
{
  struct add_to_obstack_info *info = (struct add_to_obstack_info *) data;

  if (info->check_dir && !is_directory (path, false))
    return NULL;

  if (!info->first_time)
    obstack_1grow (info->ob, PATH_SEPARATOR);

  obstack_grow (info->ob, path, strlen (path));

  info->first_time = false;
  return NULL;
}

/* Add or change the value of an environment variable, outputting the
   change to standard error if in verbose mode.  */
static void
xputenv (const char *string)
{
  if (verbose_flag)
    fnotice (stderr, "%s\n", string);
  putenv (CONST_CAST (char *, string));
}

/* Build a list of search directories from PATHS.
   PREFIX is a string to prepend to the list.
   If CHECK_DIR_P is true we ensure the directory exists.
   If DO_MULTI is true, multilib paths are output first, then
   non-multilib paths.
   This is used mostly by putenv_from_prefixes so we use `collect_obstack'.
   It is also used by the --print-search-dirs flag.  */

static char *
build_search_list (const struct path_prefix *paths, const char *prefix,
		   bool check_dir, bool do_multi)
{
  struct add_to_obstack_info info;

  info.ob = &collect_obstack;
  info.check_dir = check_dir;
  info.first_time = true;

  obstack_grow (&collect_obstack, prefix, strlen (prefix));
  obstack_1grow (&collect_obstack, '=');

  for_each_path (paths, do_multi, 0, add_to_obstack, &info);

  obstack_1grow (&collect_obstack, '\0');
  return XOBFINISH (&collect_obstack, char *);
}

/* Rebuild the COMPILER_PATH and LIBRARY_PATH environment variables
   for collect.  */

static void
putenv_from_prefixes (const struct path_prefix *paths, const char *env_var,
		      bool do_multi)
{
  xputenv (build_search_list (paths, env_var, true, do_multi));
}

/* Check whether NAME can be accessed in MODE.  This is like access,
   except that it never considers directories to be executable.  */

static int
access_check (const char *name, int mode)
{
  if (mode == X_OK)
    {
      struct stat st;

      if (stat (name, &st) < 0
	  || S_ISDIR (st.st_mode))
	return -1;
    }

  return access (name, mode);
}

/* Callback for find_a_file.  Appends the file name to the directory
   path.  If the resulting file exists in the right mode, return the
   full pathname to the file.  */

struct file_at_path_info {
  const char *name;
  const char *suffix;
  int name_len;
  int suffix_len;
  int mode;
};

static void *
file_at_path (char *path, void *data)
{
  struct file_at_path_info *info = (struct file_at_path_info *) data;
  size_t len = strlen (path);

  memcpy (path + len, info->name, info->name_len);
  len += info->name_len;

  /* Some systems have a suffix for executable files.
     So try appending that first.  */
  if (info->suffix_len)
    {
      memcpy (path + len, info->suffix, info->suffix_len + 1);
      if (access_check (path, info->mode) == 0)
	return path;
    }

  path[len] = '\0';
  if (access_check (path, info->mode) == 0)
    return path;

  return NULL;
}

/* Search for NAME using the prefix list PREFIXES.  MODE is passed to
   access to check permissions.  If DO_MULTI is true, search multilib
   paths then non-multilib paths, otherwise do not search multilib paths.
   Return 0 if not found, otherwise return its name, allocated with malloc.  */

static char *
find_a_file (const struct path_prefix *pprefix, const char *name, int mode,
	     bool do_multi)
{
  struct file_at_path_info info;

#ifdef DEFAULT_ASSEMBLER
  if (! strcmp (name, "as") && access (DEFAULT_ASSEMBLER, mode) == 0)
    return xstrdup (DEFAULT_ASSEMBLER);
#endif

#ifdef DEFAULT_LINKER
  if (! strcmp(name, "ld") && access (DEFAULT_LINKER, mode) == 0)
    return xstrdup (DEFAULT_LINKER);
#endif

  /* Determine the filename to execute (special case for absolute paths).  */

  if (IS_ABSOLUTE_PATH (name))
    {
      if (access (name, mode) == 0)
	return xstrdup (name);

      return NULL;
    }

  info.name = name;
  info.suffix = (mode & X_OK) != 0 ? HOST_EXECUTABLE_SUFFIX : "";
  info.name_len = strlen (info.name);
  info.suffix_len = strlen (info.suffix);
  info.mode = mode;

  return (char*) for_each_path (pprefix, do_multi,
				info.name_len + info.suffix_len,
				file_at_path, &info);
}

/* Ranking of prefixes in the sort list. -B prefixes are put before
   all others.  */

enum path_prefix_priority
{
  PREFIX_PRIORITY_B_OPT,
  PREFIX_PRIORITY_LAST
};

/* Add an entry for PREFIX in PLIST.  The PLIST is kept in ascending
   order according to PRIORITY.  Within each PRIORITY, new entries are
   appended.

   If WARN is nonzero, we will warn if no file is found
   through this prefix.  WARN should point to an int
   which will be set to 1 if this entry is used.

   COMPONENT is the value to be passed to update_path.

   REQUIRE_MACHINE_SUFFIX is 1 if this prefix can't be used without
   the complete value of machine_suffix.
   2 means try both machine_suffix and just_machine_suffix.  */

static void
add_prefix (struct path_prefix *pprefix, const char *prefix,
	    const char *component, /* enum prefix_priority */ int priority,
	    int require_machine_suffix, int os_multilib)
{
  struct prefix_list *pl, **prev;
  int len;

  for (prev = &pprefix->plist;
       (*prev) != NULL && (*prev)->priority <= priority;
       prev = &(*prev)->next)
    ;

  /* Keep track of the longest prefix.  */

  prefix = update_path (prefix, component);
  len = strlen (prefix);
  if (len > pprefix->max_len)
    pprefix->max_len = len;

  pl = XNEW (struct prefix_list);
  pl->prefix = prefix;
  pl->require_machine_suffix = require_machine_suffix;
  pl->priority = priority;
  pl->os_multilib = os_multilib;

  /* Insert after PREV.  */
  pl->next = (*prev);
  (*prev) = pl;
}

/* Same as add_prefix, but prepending target_system_root to prefix.  */
/* The target_system_root prefix has been relocated by gcc_exec_prefix.  */
static void
add_sysrooted_prefix (struct path_prefix *pprefix, const char *prefix,
		      const char *component,
		      /* enum prefix_priority */ int priority,
		      int require_machine_suffix, int os_multilib)
{
  if (!IS_ABSOLUTE_PATH (prefix))
    fatal_error ("system path %qs is not absolute", prefix);

  if (target_system_root)
    {
      char *sysroot_no_trailing_dir_separator = xstrdup (target_system_root);
      size_t sysroot_len = strlen (target_system_root);

      if (sysroot_len > 0
	  && target_system_root[sysroot_len - 1] == DIR_SEPARATOR)
	sysroot_no_trailing_dir_separator[sysroot_len - 1] = '\0';

      if (target_sysroot_suffix)
	prefix = concat (sysroot_no_trailing_dir_separator,
			 target_sysroot_suffix, prefix, NULL);
      else
	prefix = concat (sysroot_no_trailing_dir_separator, prefix, NULL);

      free (sysroot_no_trailing_dir_separator);

      /* We have to override this because GCC's notion of sysroot
	 moves along with GCC.  */
      component = "GCC";
    }

  add_prefix (pprefix, prefix, component, priority,
	      require_machine_suffix, os_multilib);
}

/* Execute the command specified by the arguments on the current line of spec.
   When using pipes, this includes several piped-together commands
   with `|' between them.

   Return 0 if successful, -1 if failed.  */

static int
execute (void)
{
  int i;
  int n_commands;		/* # of command.  */
  char *string;
  struct pex_obj *pex;
  struct command
  {
    const char *prog;		/* program name.  */
    const char **argv;		/* vector of args.  */
  };
  const char *arg;

  struct command *commands;	/* each command buffer with above info.  */

  gcc_assert (!processing_spec_function);

  if (wrapper_string)
    {
      string = find_a_file (&exec_prefixes,
			    argbuf[0], X_OK, false);
      if (string)
	argbuf[0] = string;
      insert_wrapper (wrapper_string);
    }

  /* Count # of piped commands.  */
  for (n_commands = 1, i = 0; argbuf.iterate (i, &arg); i++)
    if (strcmp (arg, "|") == 0)
      n_commands++;

  /* Get storage for each command.  */
  commands = (struct command *) alloca (n_commands * sizeof (struct command));

  /* Split argbuf into its separate piped processes,
     and record info about each one.
     Also search for the programs that are to be run.  */

  argbuf.safe_push (0);

  commands[0].prog = argbuf[0]; /* first command.  */
  commands[0].argv = argbuf.address ();

  if (!wrapper_string)
    {
      string = find_a_file (&exec_prefixes, commands[0].prog, X_OK, false);
      commands[0].argv[0] = (string) ? string : commands[0].argv[0];
    }

  for (n_commands = 1, i = 0; argbuf.iterate (i, &arg); i++)
    if (arg && strcmp (arg, "|") == 0)
      {				/* each command.  */
#if defined (__MSDOS__) || defined (OS2) || defined (VMS)
	fatal_error ("-pipe not supported");
#endif
	argbuf[i] = 0; /* Termination of
						     command args.  */
	commands[n_commands].prog = argbuf[i + 1];
	commands[n_commands].argv
	  = &(argbuf.address ())[i + 1];
	string = find_a_file (&exec_prefixes, commands[n_commands].prog,
			      X_OK, false);
	if (string)
	  commands[n_commands].argv[0] = string;
	n_commands++;
      }

  /* If -v, print what we are about to do, and maybe query.  */

  if (verbose_flag)
    {
      /* For help listings, put a blank line between sub-processes.  */
      if (print_help_list)
	fputc ('\n', stderr);

      /* Print each piped command as a separate line.  */
      for (i = 0; i < n_commands; i++)
	{
	  const char *const *j;

	  if (verbose_only_flag)
	    {
	      for (j = commands[i].argv; *j; j++)
		{
		  const char *p;
		  for (p = *j; *p; ++p)
		    if (!ISALNUM ((unsigned char) *p)
			&& *p != '_' && *p != '/' && *p != '-' && *p != '.')
		      break;
		  if (*p || !*j)
		    {
		      fprintf (stderr, " \"");
		      for (p = *j; *p; ++p)
			{
			  if (*p == '"' || *p == '\\' || *p == '$')
			    fputc ('\\', stderr);
			  fputc (*p, stderr);
			}
		      fputc ('"', stderr);
		    }
		  /* If it's empty, print "".  */
		  else if (!**j)
		    fprintf (stderr, " \"\"");
		  else
		    fprintf (stderr, " %s", *j);
		}
	    }
	  else
	    for (j = commands[i].argv; *j; j++)
	      /* If it's empty, print "".  */
	      if (!**j)
		fprintf (stderr, " \"\"");
	      else
		fprintf (stderr, " %s", *j);

	  /* Print a pipe symbol after all but the last command.  */
	  if (i + 1 != n_commands)
	    fprintf (stderr, " |");
	  fprintf (stderr, "\n");
	}
      fflush (stderr);
      if (verbose_only_flag != 0)
        {
	  /* verbose_only_flag should act as if the spec was
	     executed, so increment execution_count before
	     returning.  This prevents spurious warnings about
	     unused linker input files, etc.  */
	  execution_count++;
	  return 0;
        }
#ifdef DEBUG
      fnotice (stderr, "\nGo ahead? (y or n) ");
      fflush (stderr);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n')
	  ;

      if (i != 'y' && i != 'Y')
	return 0;
#endif /* DEBUG */
    }

#ifdef ENABLE_VALGRIND_CHECKING
  /* Run the each command through valgrind.  To simplify prepending the
     path to valgrind and the option "-q" (for quiet operation unless
     something triggers), we allocate a separate argv array.  */

  for (i = 0; i < n_commands; i++)
    {
      const char **argv;
      int argc;
      int j;

      for (argc = 0; commands[i].argv[argc] != NULL; argc++)
	;

      argv = XALLOCAVEC (const char *, argc + 3);

      argv[0] = VALGRIND_PATH;
      argv[1] = "-q";
      for (j = 2; j < argc + 2; j++)
	argv[j] = commands[i].argv[j - 2];
      argv[j] = NULL;

      commands[i].argv = argv;
      commands[i].prog = argv[0];
    }
#endif

  /* Run each piped subprocess.  */

  pex = pex_init (PEX_USE_PIPES | ((report_times || report_times_to_file)
				   ? PEX_RECORD_TIMES : 0),
		  progname, temp_filename);
  if (pex == NULL)
    fatal_error ("pex_init failed: %m");

  for (i = 0; i < n_commands; i++)
    {
      const char *errmsg;
      int err;
      const char *string = commands[i].argv[0];

      errmsg = pex_run (pex,
			((i + 1 == n_commands ? PEX_LAST : 0)
			 | (string == commands[i].prog ? PEX_SEARCH : 0)),
			string, CONST_CAST (char **, commands[i].argv),
			NULL, NULL, &err);
      if (errmsg != NULL)
	{
	  if (err == 0)
	    fatal_error (errmsg);
	  else
	    {
	      errno = err;
	      pfatal_with_name (errmsg);
	    }
	}

      if (string != commands[i].prog)
	free (CONST_CAST (char *, string));
    }

  execution_count++;

  /* Wait for all the subprocesses to finish.  */

  {
    int *statuses;
    struct pex_time *times = NULL;
    int ret_code = 0;

    statuses = (int *) alloca (n_commands * sizeof (int));
    if (!pex_get_status (pex, n_commands, statuses))
      fatal_error ("failed to get exit status: %m");

    if (report_times || report_times_to_file)
      {
	times = (struct pex_time *) alloca (n_commands * sizeof (struct pex_time));
	if (!pex_get_times (pex, n_commands, times))
	  fatal_error ("failed to get process times: %m");
      }

    pex_free (pex);

    for (i = 0; i < n_commands; ++i)
      {
	int status = statuses[i];

	if (WIFSIGNALED (status))
	  {
#ifdef SIGPIPE
	    /* SIGPIPE is a special case.  It happens in -pipe mode
	       when the compiler dies before the preprocessor is done,
	       or the assembler dies before the compiler is done.
	       There's generally been an error already, and this is
	       just fallout.  So don't generate another error unless
	       we would otherwise have succeeded.  */
	    if (WTERMSIG (status) == SIGPIPE
		&& (signal_count || greatest_status >= MIN_FATAL_STATUS))
	      {
		signal_count++;
		ret_code = -1;
	      }
	    else
#endif
	      internal_error ("%s (program %s)",
			      strsignal (WTERMSIG (status)), commands[i].prog);
	  }
	else if (WIFEXITED (status)
		 && WEXITSTATUS (status) >= MIN_FATAL_STATUS)
	  {
	    if (WEXITSTATUS (status) > greatest_status)
	      greatest_status = WEXITSTATUS (status);
	    ret_code = -1;
	  }

	if (report_times || report_times_to_file)
	  {
	    struct pex_time *pt = &times[i];
	    double ut, st;

	    ut = ((double) pt->user_seconds
		  + (double) pt->user_microseconds / 1.0e6);
	    st = ((double) pt->system_seconds
		  + (double) pt->system_microseconds / 1.0e6);

	    if (ut + st != 0)
	      {
		if (report_times)
		  fnotice (stderr, "# %s %.2f %.2f\n",
			   commands[i].prog, ut, st);

		if (report_times_to_file)
		  {
		    int c = 0;
		    const char *const *j;

		    fprintf (report_times_to_file, "%g %g", ut, st);

		    for (j = &commands[i].prog; *j; j = &commands[i].argv[++c])
		      {
			const char *p;
			for (p = *j; *p; ++p)
			  if (*p == '"' || *p == '\\' || *p == '$'
			      || ISSPACE (*p))
			    break;

			if (*p)
			  {
			    fprintf (report_times_to_file, " \"");
			    for (p = *j; *p; ++p)
			      {
				if (*p == '"' || *p == '\\' || *p == '$')
				  fputc ('\\', report_times_to_file);
				fputc (*p, report_times_to_file);
			      }
			    fputc ('"', report_times_to_file);
			  }
			else
			  fprintf (report_times_to_file, " %s", *j);
		      }

		    fputc ('\n', report_times_to_file);
		  }
	      }
	  }
      }

    return ret_code;
  }
}

/* Find all the switches given to us
   and make a vector describing them.
   The elements of the vector are strings, one per switch given.
   If a switch uses following arguments, then the `part1' field
   is the switch itself and the `args' field
   is a null-terminated vector containing the following arguments.
   Bits in the `live_cond' field are:
   SWITCH_LIVE to indicate this switch is true in a conditional spec.
   SWITCH_FALSE to indicate this switch is overridden by a later switch.
   SWITCH_IGNORE to indicate this switch should be ignored (used in %<S).
   SWITCH_IGNORE_PERMANENTLY to indicate this switch should be ignored
   in all do_spec calls afterwards.  Used for %<S from self specs.
   The `validated' field is nonzero if any spec has looked at this switch;
   if it remains zero at the end of the run, it must be meaningless.  */

#define SWITCH_LIVE    			(1 << 0)
#define SWITCH_FALSE   			(1 << 1)
#define SWITCH_IGNORE			(1 << 2)
#define SWITCH_IGNORE_PERMANENTLY	(1 << 3)
#define SWITCH_KEEP_FOR_GCC		(1 << 4)

struct switchstr
{
  const char *part1;
  const char **args;
  unsigned int live_cond;
  bool known;
  bool validated;
  bool ordering;
};

static struct switchstr *switches;

static int n_switches;

static int n_switches_alloc;

/* Set to zero if -fcompare-debug is disabled, positive if it's
   enabled and we're running the first compilation, negative if it's
   enabled and we're running the second compilation.  For most of the
   time, it's in the range -1..1, but it can be temporarily set to 2
   or 3 to indicate that the -fcompare-debug flags didn't come from
   the command-line, but rather from the GCC_COMPARE_DEBUG environment
   variable, until a synthesized -fcompare-debug flag is added to the
   command line.  */
int compare_debug;

/* Set to nonzero if we've seen the -fcompare-debug-second flag.  */
int compare_debug_second;

/* Set to the flags that should be passed to the second compilation in
   a -fcompare-debug compilation.  */
const char *compare_debug_opt;

static struct switchstr *switches_debug_check[2];

static int n_switches_debug_check[2];

static int n_switches_alloc_debug_check[2];

static char *debug_check_temp_file[2];

/* Language is one of three things:

   1) The name of a real programming language.
   2) NULL, indicating that no one has figured out
   what it is yet.
   3) '*', indicating that the file should be passed
   to the linker.  */
struct infile
{
  const char *name;
  const char *language;
  struct compiler *incompiler;
  bool compiled;
  bool preprocessed;
};

/* Also a vector of input files specified.  */

static struct infile *infiles;

int n_infiles;

static int n_infiles_alloc;

/* True if multiple input files are being compiled to a single
   assembly file.  */

static bool combine_inputs;

/* This counts the number of libraries added by lang_specific_driver, so that
   we can tell if there were any user supplied any files or libraries.  */

static int added_libraries;

/* And a vector of corresponding output files is made up later.  */

const char **outfiles;

#if defined(HAVE_TARGET_OBJECT_SUFFIX) || defined(HAVE_TARGET_EXECUTABLE_SUFFIX)

/* Convert NAME to a new name if it is the standard suffix.  DO_EXE
   is true if we should look for an executable suffix.  DO_OBJ
   is true if we should look for an object suffix.  */

static const char *
convert_filename (const char *name, int do_exe ATTRIBUTE_UNUSED,
		  int do_obj ATTRIBUTE_UNUSED)
{
#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
  int i;
#endif
  int len;

  if (name == NULL)
    return NULL;

  len = strlen (name);

#ifdef HAVE_TARGET_OBJECT_SUFFIX
  /* Convert x.o to x.obj if TARGET_OBJECT_SUFFIX is ".obj".  */
  if (do_obj && len > 2
      && name[len - 2] == '.'
      && name[len - 1] == 'o')
    {
      obstack_grow (&obstack, name, len - 2);
      obstack_grow0 (&obstack, TARGET_OBJECT_SUFFIX, strlen (TARGET_OBJECT_SUFFIX));
      name = XOBFINISH (&obstack, const char *);
    }
#endif

#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
  /* If there is no filetype, make it the executable suffix (which includes
     the ".").  But don't get confused if we have just "-o".  */
  if (! do_exe || TARGET_EXECUTABLE_SUFFIX[0] == 0 || (len == 2 && name[0] == '-'))
    return name;

  for (i = len - 1; i >= 0; i--)
    if (IS_DIR_SEPARATOR (name[i]))
      break;

  for (i++; i < len; i++)
    if (name[i] == '.')
      return name;

  obstack_grow (&obstack, name, len);
  obstack_grow0 (&obstack, TARGET_EXECUTABLE_SUFFIX,
		 strlen (TARGET_EXECUTABLE_SUFFIX));
  name = XOBFINISH (&obstack, const char *);
#endif

  return name;
}
#endif

/* Display the command line switches accepted by gcc.  */
static void
display_help (void)
{
  printf (_("Usage: %s [options] file...\n"), progname);
  fputs (_("Options:\n"), stdout);

  fputs (_("  -pass-exit-codes         Exit with highest error code from a phase\n"), stdout);
  fputs (_("  --help                   Display this information\n"), stdout);
  fputs (_("  --target-help            Display target specific command line options\n"), stdout);
  fputs (_("  --help={common|optimizers|params|target|warnings|[^]{joined|separate|undocumented}}[,...]\n"), stdout);
  fputs (_("                           Display specific types of command line options\n"), stdout);
  if (! verbose_flag)
    fputs (_("  (Use '-v --help' to display command line options of sub-processes)\n"), stdout);
  fputs (_("  --version                Display compiler version information\n"), stdout);
  fputs (_("  -dumpspecs               Display all of the built in spec strings\n"), stdout);
  fputs (_("  -dumpversion             Display the version of the compiler\n"), stdout);
  fputs (_("  -dumpmachine             Display the compiler's target processor\n"), stdout);
  fputs (_("  -print-search-dirs       Display the directories in the compiler's search path\n"), stdout);
  fputs (_("  -print-libgcc-file-name  Display the name of the compiler's companion library\n"), stdout);
  fputs (_("  -print-file-name=<lib>   Display the full path to library <lib>\n"), stdout);
  fputs (_("  -print-prog-name=<prog>  Display the full path to compiler component <prog>\n"), stdout);
  fputs (_("\
  -print-multiarch         Display the target's normalized GNU triplet, used as\n\
                           a component in the library path\n"), stdout);
  fputs (_("  -print-multi-directory   Display the root directory for versions of libgcc\n"), stdout);
  fputs (_("\
  -print-multi-lib         Display the mapping between command line options and\n\
                           multiple library search directories\n"), stdout);
  fputs (_("  -print-multi-os-directory Display the relative path to OS libraries\n"), stdout);
  fputs (_("  -print-sysroot           Display the target libraries directory\n"), stdout);
  fputs (_("  -print-sysroot-headers-suffix Display the sysroot suffix used to find headers\n"), stdout);
  fputs (_("  -Wa,<options>            Pass comma-separated <options> on to the assembler\n"), stdout);
  fputs (_("  -Wp,<options>            Pass comma-separated <options> on to the preprocessor\n"), stdout);
  fputs (_("  -Wl,<options>            Pass comma-separated <options> on to the linker\n"), stdout);
  fputs (_("  -Xassembler <arg>        Pass <arg> on to the assembler\n"), stdout);
  fputs (_("  -Xpreprocessor <arg>     Pass <arg> on to the preprocessor\n"), stdout);
  fputs (_("  -Xlinker <arg>           Pass <arg> on to the linker\n"), stdout);
  fputs (_("  -save-temps              Do not delete intermediate files\n"), stdout);
  fputs (_("  -save-temps=<arg>        Do not delete intermediate files\n"), stdout);
  fputs (_("\
  -no-canonical-prefixes   Do not canonicalize paths when building relative\n\
                           prefixes to other gcc components\n"), stdout);
  fputs (_("  -pipe                    Use pipes rather than intermediate files\n"), stdout);
  fputs (_("  -time                    Time the execution of each subprocess\n"), stdout);
  fputs (_("  -specs=<file>            Override built-in specs with the contents of <file>\n"), stdout);
  fputs (_("  -std=<standard>          Assume that the input sources are for <standard>\n"), stdout);
  fputs (_("\
  --sysroot=<directory>    Use <directory> as the root directory for headers\n\
                           and libraries\n"), stdout);
  fputs (_("  -B <directory>           Add <directory> to the compiler's search paths\n"), stdout);
  fputs (_("  -v                       Display the programs invoked by the compiler\n"), stdout);
  fputs (_("  -###                     Like -v but options quoted and commands not executed\n"), stdout);
  fputs (_("  -E                       Preprocess only; do not compile, assemble or link\n"), stdout);
  fputs (_("  -S                       Compile only; do not assemble or link\n"), stdout);
  fputs (_("  -c                       Compile and assemble, but do not link\n"), stdout);
  fputs (_("  -o <file>                Place the output into <file>\n"), stdout);
  fputs (_("  -pie                     Create a position independent executable\n"), stdout);
  fputs (_("  -shared                  Create a shared library\n"), stdout);
  fputs (_("\
  -x <language>            Specify the language of the following input files\n\
                           Permissible languages include: c c++ assembler none\n\
                           'none' means revert to the default behavior of\n\
                           guessing the language based on the file's extension\n\
"), stdout);

  printf (_("\
\nOptions starting with -g, -f, -m, -O, -W, or --param are automatically\n\
 passed on to the various sub-processes invoked by %s.  In order to pass\n\
 other options on to these processes the -W<letter> options must be used.\n\
"), progname);

  /* The rest of the options are displayed by invocations of the various
     sub-processes.  */
}

static void
add_preprocessor_option (const char *option, int len)
{
  preprocessor_options.safe_push (save_string (option, len));
}

static void
add_assembler_option (const char *option, int len)
{
  assembler_options.safe_push (save_string (option, len));
}

static void
add_linker_option (const char *option, int len)
{
  linker_options.safe_push (save_string (option, len));
}

/* Allocate space for an input file in infiles.  */

static void
alloc_infile (void)
{
  if (n_infiles_alloc == 0)
    {
      n_infiles_alloc = 16;
      infiles = XNEWVEC (struct infile, n_infiles_alloc);
    }
  else if (n_infiles_alloc == n_infiles)
    {
      n_infiles_alloc *= 2;
      infiles = XRESIZEVEC (struct infile, infiles, n_infiles_alloc);
    }
}

/* Store an input file with the given NAME and LANGUAGE in
   infiles.  */

static void
add_infile (const char *name, const char *language)
{
  alloc_infile ();
  infiles[n_infiles].name = name;
  infiles[n_infiles++].language = language;
}

/* Allocate space for a switch in switches.  */

static void
alloc_switch (void)
{
  if (n_switches_alloc == 0)
    {
      n_switches_alloc = 16;
      switches = XNEWVEC (struct switchstr, n_switches_alloc);
    }
  else if (n_switches_alloc == n_switches)
    {
      n_switches_alloc *= 2;
      switches = XRESIZEVEC (struct switchstr, switches, n_switches_alloc);
    }
}

/* Save an option OPT with N_ARGS arguments in array ARGS, marking it
   as validated if VALIDATED and KNOWN if it is an internal switch.  */

static void
save_switch (const char *opt, size_t n_args, const char *const *args,
	     bool validated, bool known)
{
  alloc_switch ();
  switches[n_switches].part1 = opt + 1;
  if (n_args == 0)
    switches[n_switches].args = 0;
  else
    {
      switches[n_switches].args = XNEWVEC (const char *, n_args + 1);
      memcpy (switches[n_switches].args, args, n_args * sizeof (const char *));
      switches[n_switches].args[n_args] = NULL;
    }

  switches[n_switches].live_cond = 0;
  switches[n_switches].validated = validated;
  switches[n_switches].known = known;
  switches[n_switches].ordering = 0;
  n_switches++;
}

/* Handle an option DECODED that is unknown to the option-processing
   machinery.  */

static bool
driver_unknown_option_callback (const struct cl_decoded_option *decoded)
{
  const char *opt = decoded->arg;
  if (opt[1] == 'W' && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-'
      && !(decoded->errors & CL_ERR_NEGATIVE))
    {
      /* Leave unknown -Wno-* options for the compiler proper, to be
	 diagnosed only if there are warnings.  */
      save_switch (decoded->canonical_option[0],
		   decoded->canonical_option_num_elements - 1,
		   &decoded->canonical_option[1], false, true);
      return false;
    }
  if (decoded->opt_index == OPT_SPECIAL_unknown)
    {
      /* Give it a chance to define it a a spec file.  */
      save_switch (decoded->canonical_option[0],
		   decoded->canonical_option_num_elements - 1,
		   &decoded->canonical_option[1], false, false);
      return false;
    }
  else
    return true;
}

/* Handle an option DECODED that is not marked as CL_DRIVER.
   LANG_MASK will always be CL_DRIVER.  */

static void
driver_wrong_lang_callback (const struct cl_decoded_option *decoded,
			    unsigned int lang_mask ATTRIBUTE_UNUSED)
{
  /* At this point, non-driver options are accepted (and expected to
     be passed down by specs) unless marked to be rejected by the
     driver.  Options to be rejected by the driver but accepted by the
     compilers proper are treated just like completely unknown
     options.  */
  const struct cl_option *option = &cl_options[decoded->opt_index];

  if (option->cl_reject_driver)
    error ("unrecognized command line option %qs",
	   decoded->orig_option_with_args_text);
  else
    save_switch (decoded->canonical_option[0],
		 decoded->canonical_option_num_elements - 1,
		 &decoded->canonical_option[1], false, true);
}

static const char *spec_lang = 0;
static int last_language_n_infiles;

/* Handle a driver option; arguments and return value as for
   handle_option.  */

static bool
driver_handle_option (struct gcc_options *opts,
		      struct gcc_options *opts_set,
		      const struct cl_decoded_option *decoded,
		      unsigned int lang_mask ATTRIBUTE_UNUSED, int kind,
		      location_t loc,
		      const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED,
		      diagnostic_context *dc)
{
  size_t opt_index = decoded->opt_index;
  const char *arg = decoded->arg;
  const char *compare_debug_replacement_opt;
  int value = decoded->value;
  bool validated = false;
  bool do_save = true;

  gcc_assert (opts == &global_options);
  gcc_assert (opts_set == &global_options_set);
  gcc_assert (kind == DK_UNSPECIFIED);
  gcc_assert (loc == UNKNOWN_LOCATION);
  gcc_assert (dc == global_dc);

  switch (opt_index)
    {
    case OPT_dumpspecs:
      {
	struct spec_list *sl;
	init_spec ();
	for (sl = specs; sl; sl = sl->next)
	  printf ("*%s:\n%s\n\n", sl->name, *(sl->ptr_spec));
	if (link_command_spec)
	  printf ("*link_command:\n%s\n\n", link_command_spec);
	exit (0);
      }

    case OPT_dumpversion:
      printf ("%s\n", spec_version);
      exit (0);

    case OPT_dumpmachine:
      printf ("%s\n", spec_machine);
      exit (0);

    case OPT__version:
      print_version = 1;

      /* CPP driver cannot obtain switch from cc1_options.  */
      if (is_cpp_driver)
	add_preprocessor_option ("--version", strlen ("--version"));
      add_assembler_option ("--version", strlen ("--version"));
      add_linker_option ("--version", strlen ("--version"));
      break;

    case OPT__help:
      print_help_list = 1;

      /* CPP driver cannot obtain switch from cc1_options.  */
      if (is_cpp_driver)
	add_preprocessor_option ("--help", 6);
      add_assembler_option ("--help", 6);
      add_linker_option ("--help", 6);
      break;

    case OPT__help_:
      print_subprocess_help = 2;
      break;

    case OPT__target_help:
      print_subprocess_help = 1;

      /* CPP driver cannot obtain switch from cc1_options.  */
      if (is_cpp_driver)
	add_preprocessor_option ("--target-help", 13);
      add_assembler_option ("--target-help", 13);
      add_linker_option ("--target-help", 13);
      break;

    case OPT__no_sysroot_suffix:
    case OPT_pass_exit_codes:
    case OPT_print_search_dirs:
    case OPT_print_file_name_:
    case OPT_print_prog_name_:
    case OPT_print_multi_lib:
    case OPT_print_multi_directory:
    case OPT_print_sysroot:
    case OPT_print_multi_os_directory:
    case OPT_print_multiarch:
    case OPT_print_sysroot_headers_suffix:
    case OPT_time:
    case OPT_wrapper:
      /* These options set the variables specified in common.opt
	 automatically, and do not need to be saved for spec
	 processing.  */
      do_save = false;
      break;

    case OPT_print_libgcc_file_name:
      print_file_name = "libgcc.a";
      do_save = false;
      break;

    case OPT_fcompare_debug_second:
      compare_debug_second = 1;
      break;

    case OPT_fcompare_debug:
      switch (value)
	{
	case 0:
	  compare_debug_replacement_opt = "-fcompare-debug=";
	  arg = "";
	  goto compare_debug_with_arg;

	case 1:
	  compare_debug_replacement_opt = "-fcompare-debug=-gtoggle";
	  arg = "-gtoggle";
	  goto compare_debug_with_arg;

	default:
	  gcc_unreachable ();
	}
      break;

    case OPT_fcompare_debug_:
      compare_debug_replacement_opt = decoded->canonical_option[0];
    compare_debug_with_arg:
      gcc_assert (decoded->canonical_option_num_elements == 1);
      gcc_assert (arg != NULL);
      if (*arg)
	compare_debug = 1;
      else
	compare_debug = -1;
      if (compare_debug < 0)
	compare_debug_opt = NULL;
      else
	compare_debug_opt = arg;
      save_switch (compare_debug_replacement_opt, 0, NULL, validated, true);
      return true;

    case OPT_Wa_:
      {
	int prev, j;
	/* Pass the rest of this option to the assembler.  */

	/* Split the argument at commas.  */
	prev = 0;
	for (j = 0; arg[j]; j++)
	  if (arg[j] == ',')
	    {
	      add_assembler_option (arg + prev, j - prev);
	      prev = j + 1;
	    }

	/* Record the part after the last comma.  */
	add_assembler_option (arg + prev, j - prev);
      }
      do_save = false;
      break;

    case OPT_Wp_:
      {
	int prev, j;
	/* Pass the rest of this option to the preprocessor.  */

	/* Split the argument at commas.  */
	prev = 0;
	for (j = 0; arg[j]; j++)
	  if (arg[j] == ',')
	    {
	      add_preprocessor_option (arg + prev, j - prev);
	      prev = j + 1;
	    }

	/* Record the part after the last comma.  */
	add_preprocessor_option (arg + prev, j - prev);
      }
      do_save = false;
      break;

    case OPT_Wl_:
      {
	int prev, j;
	/* Split the argument at commas.  */
	prev = 0;
	for (j = 0; arg[j]; j++)
	  if (arg[j] == ',')
	    {
	      add_infile (save_string (arg + prev, j - prev), "*");
	      prev = j + 1;
	    }
	/* Record the part after the last comma.  */
	add_infile (arg + prev, "*");
      }
      do_save = false;
      break;

    case OPT_Xlinker:
      add_infile (arg, "*");
      do_save = false;
      break;

    case OPT_Xpreprocessor:
      add_preprocessor_option (arg, strlen (arg));
      do_save = false;
      break;

    case OPT_Xassembler:
      add_assembler_option (arg, strlen (arg));
      do_save = false;
      break;

    case OPT_l:
      /* POSIX allows separation of -l and the lib arg; canonicalize
	 by concatenating -l with its arg */
      add_infile (concat ("-l", arg, NULL), "*");
      do_save = false;
      break;

    case OPT_L:
      /* Similarly, canonicalize -L for linkers that may not accept
	 separate arguments.  */
      save_switch (concat ("-L", arg, NULL), 0, NULL, validated, true);
      return true;

    case OPT_F:
      /* Likewise -F.  */
      save_switch (concat ("-F", arg, NULL), 0, NULL, validated, true);
      return true;

    case OPT_save_temps:
      save_temps_flag = SAVE_TEMPS_CWD;
      validated = true;
      break;

    case OPT_save_temps_:
      if (strcmp (arg, "cwd") == 0)
	save_temps_flag = SAVE_TEMPS_CWD;
      else if (strcmp (arg, "obj") == 0
	       || strcmp (arg, "object") == 0)
	save_temps_flag = SAVE_TEMPS_OBJ;
      else
	fatal_error ("%qs is an unknown -save-temps option",
		     decoded->orig_option_with_args_text);
      break;

    case OPT_no_canonical_prefixes:
      /* Already handled as a special case, so ignored here.  */
      do_save = false;
      break;

    case OPT_pipe:
      validated = true;
      /* These options set the variables specified in common.opt
	 automatically, but do need to be saved for spec
	 processing.  */
      break;

    case OPT_specs_:
      {
	struct user_specs *user = XNEW (struct user_specs);

	user->next = (struct user_specs *) 0;
	user->filename = arg;
	if (user_specs_tail)
	  user_specs_tail->next = user;
	else
	  user_specs_head = user;
	user_specs_tail = user;
      }
      validated = true;
      break;

    case OPT__sysroot_:
      target_system_root = arg;
      target_system_root_changed = 1;
      do_save = false;
      break;

    case OPT_time_:
      if (report_times_to_file)
	fclose (report_times_to_file);
      report_times_to_file = fopen (arg, "a");
      do_save = false;
      break;

    case OPT____:
      /* "-###"
	 This is similar to -v except that there is no execution
	 of the commands and the echoed arguments are quoted.  It
	 is intended for use in shell scripts to capture the
	 driver-generated command line.  */
      verbose_only_flag++;
      verbose_flag = 1;
      do_save = false;
      break;

    case OPT_B:
      {
	size_t len = strlen (arg);

	/* Catch the case where the user has forgotten to append a
	   directory separator to the path.  Note, they may be using
	   -B to add an executable name prefix, eg "i386-elf-", in
	   order to distinguish between multiple installations of
	   GCC in the same directory.  Hence we must check to see
	   if appending a directory separator actually makes a
	   valid directory name.  */
	if (!IS_DIR_SEPARATOR (arg[len - 1])
	    && is_directory (arg, false))
	  {
	    char *tmp = XNEWVEC (char, len + 2);
	    strcpy (tmp, arg);
	    tmp[len] = DIR_SEPARATOR;
	    tmp[++len] = 0;
	    arg = tmp;
	  }

	add_prefix (&exec_prefixes, arg, NULL,
		    PREFIX_PRIORITY_B_OPT, 0, 0);
	add_prefix (&startfile_prefixes, arg, NULL,
		    PREFIX_PRIORITY_B_OPT, 0, 0);
	add_prefix (&include_prefixes, arg, NULL,
		    PREFIX_PRIORITY_B_OPT, 0, 0);
      }
      validated = true;
      break;

    case OPT_x:
      spec_lang = arg;
      if (!strcmp (spec_lang, "none"))
	/* Suppress the warning if -xnone comes after the last input
	   file, because alternate command interfaces like g++ might
	   find it useful to place -xnone after each input file.  */
	spec_lang = 0;
      else
	last_language_n_infiles = n_infiles;
      do_save = false;
      break;

    case OPT_o:
      have_o = 1;
#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX) || defined(HAVE_TARGET_OBJECT_SUFFIX)
      arg = convert_filename (arg, ! have_c, 0);
#endif
      /* Save the output name in case -save-temps=obj was used.  */
      save_temps_prefix = xstrdup (arg);
      /* On some systems, ld cannot handle "-o" without a space.  So
	 split the option from its argument.  */
      save_switch ("-o", 1, &arg, validated, true);
      return true;

    case OPT_static_libgcc:
    case OPT_shared_libgcc:
    case OPT_static_libgfortran:
    case OPT_static_libstdc__:
      /* These are always valid, since gcc.c itself understands the
	 first two, gfortranspec.c understands -static-libgfortran and
	 g++spec.c understands -static-libstdc++ */
      validated = true;
      break;

    default:
      /* Various driver options need no special processing at this
	 point, having been handled in a prescan above or being
	 handled by specs.  */
      break;
    }

  if (do_save)
    save_switch (decoded->canonical_option[0],
		 decoded->canonical_option_num_elements - 1,
		 &decoded->canonical_option[1], validated, true);
  return true;
}

/* Put the driver's standard set of option handlers in *HANDLERS.  */

static void
set_option_handlers (struct cl_option_handlers *handlers)
{
  handlers->unknown_option_callback = driver_unknown_option_callback;
  handlers->wrong_lang_callback = driver_wrong_lang_callback;
  handlers->num_handlers = 3;
  handlers->handlers[0].handler = driver_handle_option;
  handlers->handlers[0].mask = CL_DRIVER;
  handlers->handlers[1].handler = common_handle_option;
  handlers->handlers[1].mask = CL_COMMON;
  handlers->handlers[2].handler = target_handle_option;
  handlers->handlers[2].mask = CL_TARGET;
}

/* Create the vector `switches' and its contents.
   Store its length in `n_switches'.  */

static void
process_command (unsigned int decoded_options_count,
		 struct cl_decoded_option *decoded_options)
{
  const char *temp;
  char *temp1;
  char *tooldir_prefix, *tooldir_prefix2;
  char *(*get_relative_prefix) (const char *, const char *,
				const char *) = NULL;
  struct cl_option_handlers handlers;
  unsigned int j;

  gcc_exec_prefix = getenv ("GCC_EXEC_PREFIX");

  n_switches = 0;
  n_infiles = 0;
  added_libraries = 0;

  /* Figure compiler version from version string.  */

  compiler_version = temp1 = xstrdup (version_string);

  for (; *temp1; ++temp1)
    {
      if (*temp1 == ' ')
	{
	  *temp1 = '\0';
	  break;
	}
    }

  /* Handle any -no-canonical-prefixes flag early, to assign the function
     that builds relative prefixes.  This function creates default search
     paths that are needed later in normal option handling.  */

  for (j = 1; j < decoded_options_count; j++)
    {
      if (decoded_options[j].opt_index == OPT_no_canonical_prefixes)
	{
	  get_relative_prefix = make_relative_prefix_ignore_links;
	  break;
	}
    }
  if (! get_relative_prefix)
    get_relative_prefix = make_relative_prefix;

  /* Set up the default search paths.  If there is no GCC_EXEC_PREFIX,
     see if we can create it from the pathname specified in
     decoded_options[0].arg.  */

  gcc_libexec_prefix = standard_libexec_prefix;
#ifndef VMS
  /* FIXME: make_relative_prefix doesn't yet work for VMS.  */
  if (!gcc_exec_prefix)
    {
      gcc_exec_prefix = get_relative_prefix (decoded_options[0].arg,
					     standard_bindir_prefix,
					     standard_exec_prefix);
      gcc_libexec_prefix = get_relative_prefix (decoded_options[0].arg,
					     standard_bindir_prefix,
					     standard_libexec_prefix);
      if (gcc_exec_prefix)
	xputenv (concat ("GCC_EXEC_PREFIX=", gcc_exec_prefix, NULL));
    }
  else
    {
      /* make_relative_prefix requires a program name, but
	 GCC_EXEC_PREFIX is typically a directory name with a trailing
	 / (which is ignored by make_relative_prefix), so append a
	 program name.  */
      char *tmp_prefix = concat (gcc_exec_prefix, "gcc", NULL);
      gcc_libexec_prefix = get_relative_prefix (tmp_prefix,
						standard_exec_prefix,
						standard_libexec_prefix);

      /* The path is unrelocated, so fallback to the original setting.  */
      if (!gcc_libexec_prefix)
	gcc_libexec_prefix = standard_libexec_prefix;

      free (tmp_prefix);
    }
#else
#endif
  /* From this point onward, gcc_exec_prefix is non-null if the toolchain
     is relocated. The toolchain was either relocated using GCC_EXEC_PREFIX
     or an automatically created GCC_EXEC_PREFIX from
     decoded_options[0].arg.  */

  /* Do language-specific adjustment/addition of flags.  */
  lang_specific_driver (&decoded_options, &decoded_options_count,
			&added_libraries);

  if (gcc_exec_prefix)
    {
      int len = strlen (gcc_exec_prefix);

      if (len > (int) sizeof ("/lib/gcc/") - 1
	  && (IS_DIR_SEPARATOR (gcc_exec_prefix[len-1])))
	{
	  temp = gcc_exec_prefix + len - sizeof ("/lib/gcc/") + 1;
	  if (IS_DIR_SEPARATOR (*temp)
	      && filename_ncmp (temp + 1, "lib", 3) == 0
	      && IS_DIR_SEPARATOR (temp[4])
	      && filename_ncmp (temp + 5, "gcc", 3) == 0)
	    len -= sizeof ("/lib/gcc/") - 1;
	}

      set_std_prefix (gcc_exec_prefix, len);
      add_prefix (&exec_prefixes, gcc_libexec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 0, 0);
      add_prefix (&startfile_prefixes, gcc_exec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 0, 0);
    }

  /* COMPILER_PATH and LIBRARY_PATH have values
     that are lists of directory names with colons.  */

  temp = getenv ("COMPILER_PATH");
  if (temp)
    {
      const char *startp, *endp;
      char *nstore = (char *) alloca (strlen (temp) + 3);

      startp = endp = temp;
      while (1)
	{
	  if (*endp == PATH_SEPARATOR || *endp == 0)
	    {
	      strncpy (nstore, startp, endp - startp);
	      if (endp == startp)
		strcpy (nstore, concat (".", dir_separator_str, NULL));
	      else if (!IS_DIR_SEPARATOR (endp[-1]))
		{
		  nstore[endp - startp] = DIR_SEPARATOR;
		  nstore[endp - startp + 1] = 0;
		}
	      else
		nstore[endp - startp] = 0;
	      add_prefix (&exec_prefixes, nstore, 0,
			  PREFIX_PRIORITY_LAST, 0, 0);
	      add_prefix (&include_prefixes, nstore, 0,
			  PREFIX_PRIORITY_LAST, 0, 0);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  temp = getenv (LIBRARY_PATH_ENV);
  if (temp && *cross_compile == '0')
    {
      const char *startp, *endp;
      char *nstore = (char *) alloca (strlen (temp) + 3);

      startp = endp = temp;
      while (1)
	{
	  if (*endp == PATH_SEPARATOR || *endp == 0)
	    {
	      strncpy (nstore, startp, endp - startp);
	      if (endp == startp)
		strcpy (nstore, concat (".", dir_separator_str, NULL));
	      else if (!IS_DIR_SEPARATOR (endp[-1]))
		{
		  nstore[endp - startp] = DIR_SEPARATOR;
		  nstore[endp - startp + 1] = 0;
		}
	      else
		nstore[endp - startp] = 0;
	      add_prefix (&startfile_prefixes, nstore, NULL,
			  PREFIX_PRIORITY_LAST, 0, 1);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  /* Use LPATH like LIBRARY_PATH (for the CMU build program).  */
  temp = getenv ("LPATH");
  if (temp && *cross_compile == '0')
    {
      const char *startp, *endp;
      char *nstore = (char *) alloca (strlen (temp) + 3);

      startp = endp = temp;
      while (1)
	{
	  if (*endp == PATH_SEPARATOR || *endp == 0)
	    {
	      strncpy (nstore, startp, endp - startp);
	      if (endp == startp)
		strcpy (nstore, concat (".", dir_separator_str, NULL));
	      else if (!IS_DIR_SEPARATOR (endp[-1]))
		{
		  nstore[endp - startp] = DIR_SEPARATOR;
		  nstore[endp - startp + 1] = 0;
		}
	      else
		nstore[endp - startp] = 0;
	      add_prefix (&startfile_prefixes, nstore, NULL,
			  PREFIX_PRIORITY_LAST, 0, 1);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  /* Process the options and store input files and switches in their
     vectors.  */

  last_language_n_infiles = -1;

  set_option_handlers (&handlers);

  for (j = 1; j < decoded_options_count; j++)
    {
      switch (decoded_options[j].opt_index)
	{
	case OPT_S:
	case OPT_c:
	case OPT_E:
	  have_c = 1;
	  break;
	}
      if (have_c)
	break;
    }

  for (j = 1; j < decoded_options_count; j++)
    {
      if (decoded_options[j].opt_index == OPT_SPECIAL_input_file)
	{
	  const char *arg = decoded_options[j].arg;
          const char *p = strrchr (arg, '@');
          char *fname;
	  long offset;
	  int consumed;
#ifdef HAVE_TARGET_OBJECT_SUFFIX
	  arg = convert_filename (arg, 0, access (arg, F_OK));
#endif
	  /* For LTO static archive support we handle input file
	     specifications that are composed of a filename and
	     an offset like FNAME@OFFSET.  */
	  if (p
	      && p != arg
	      && sscanf (p, "@%li%n", &offset, &consumed) >= 1
	      && strlen (p) == (unsigned int)consumed)
	    {
              fname = (char *)xmalloc (p - arg + 1);
              memcpy (fname, arg, p - arg);
              fname[p - arg] = '\0';
	      /* Only accept non-stdin and existing FNAME parts, otherwise
		 try with the full name.  */
	      if (strcmp (fname, "-") == 0 || access (fname, F_OK) < 0)
		{
		  free (fname);
		  fname = xstrdup (arg);
		}
	    }
	  else
	    fname = xstrdup (arg);

          if (strcmp (fname, "-") != 0 && access (fname, F_OK) < 0)
	    perror_with_name (fname);
          else
	    add_infile (arg, spec_lang);

          free (fname);
	  continue;
	}

      read_cmdline_option (&global_options, &global_options_set,
			   decoded_options + j, UNKNOWN_LOCATION,
			   CL_DRIVER, &handlers, global_dc);
    }

  /* If -save-temps=obj and -o name, create the prefix to use for %b.
     Otherwise just make -save-temps=obj the same as -save-temps=cwd.  */
  if (save_temps_flag == SAVE_TEMPS_OBJ && save_temps_prefix != NULL)
    {
      save_temps_length = strlen (save_temps_prefix);
      temp = strrchr (lbasename (save_temps_prefix), '.');
      if (temp)
	{
	  save_temps_length -= strlen (temp);
	  save_temps_prefix[save_temps_length] = '\0';
	}

    }
  else if (save_temps_prefix != NULL)
    {
      free (save_temps_prefix);
      save_temps_prefix = NULL;
    }

  if (save_temps_flag && use_pipes)
    {
      /* -save-temps overrides -pipe, so that temp files are produced */
      if (save_temps_flag)
	warning (0, "-pipe ignored because -save-temps specified");
      use_pipes = 0;
    }

  if (!compare_debug)
    {
      const char *gcd = getenv ("GCC_COMPARE_DEBUG");

      if (gcd && gcd[0] == '-')
	{
	  compare_debug = 2;
	  compare_debug_opt = gcd;
	}
      else if (gcd && *gcd && strcmp (gcd, "0"))
	{
	  compare_debug = 3;
	  compare_debug_opt = "-gtoggle";
	}
    }
  else if (compare_debug < 0)
    {
      compare_debug = 0;
      gcc_assert (!compare_debug_opt);
    }

  /* Set up the search paths.  We add directories that we expect to
     contain GNU Toolchain components before directories specified by
     the machine description so that we will find GNU components (like
     the GNU assembler) before those of the host system.  */

  /* If we don't know where the toolchain has been installed, use the
     configured-in locations.  */
  if (!gcc_exec_prefix)
    {
#ifndef OS2
      add_prefix (&exec_prefixes, standard_libexec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 1, 0);
      add_prefix (&exec_prefixes, standard_libexec_prefix, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 2, 0);
      add_prefix (&exec_prefixes, standard_exec_prefix, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 2, 0);
#endif
      add_prefix (&startfile_prefixes, standard_exec_prefix, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 1, 0);
    }

  gcc_assert (!IS_ABSOLUTE_PATH (tooldir_base_prefix));
  tooldir_prefix2 = concat (tooldir_base_prefix, spec_machine,
			    dir_separator_str, NULL);

  /* Look for tools relative to the location from which the driver is
     running, or, if that is not available, the configured prefix.  */
  tooldir_prefix
    = concat (gcc_exec_prefix ? gcc_exec_prefix : standard_exec_prefix,
	      spec_machine, dir_separator_str,
	      spec_version, dir_separator_str, tooldir_prefix2, NULL);
  free (tooldir_prefix2);

  add_prefix (&exec_prefixes,
	      concat (tooldir_prefix, "bin", dir_separator_str, NULL),
	      "BINUTILS", PREFIX_PRIORITY_LAST, 0, 0);
  add_prefix (&startfile_prefixes,
	      concat (tooldir_prefix, "lib", dir_separator_str, NULL),
	      "BINUTILS", PREFIX_PRIORITY_LAST, 0, 1);
  free (tooldir_prefix);

#if defined(TARGET_SYSTEM_ROOT_RELOCATABLE) && !defined(VMS)
  /* If the normal TARGET_SYSTEM_ROOT is inside of $exec_prefix,
     then consider it to relocate with the rest of the GCC installation
     if GCC_EXEC_PREFIX is set.
     ``make_relative_prefix'' is not compiled for VMS, so don't call it.  */
  if (target_system_root && !target_system_root_changed && gcc_exec_prefix)
    {
      char *tmp_prefix = get_relative_prefix (decoded_options[0].arg,
					      standard_bindir_prefix,
					      target_system_root);
      if (tmp_prefix && access_check (tmp_prefix, F_OK) == 0)
	{
	  target_system_root = tmp_prefix;
	  target_system_root_changed = 1;
	}
    }
#endif

  /* More prefixes are enabled in main, after we read the specs file
     and determine whether this is cross-compilation or not.  */

  if (n_infiles == last_language_n_infiles && spec_lang != 0)
    warning (0, "%<-x %s%> after last input file has no effect", spec_lang);

  /* Synthesize -fcompare-debug flag from the GCC_COMPARE_DEBUG
     environment variable.  */
  if (compare_debug == 2 || compare_debug == 3)
    {
      const char *opt = concat ("-fcompare-debug=", compare_debug_opt, NULL);
      save_switch (opt, 0, NULL, false, true);
      compare_debug = 1;
    }

  /* Ensure we only invoke each subprocess once.  */
  if (print_subprocess_help || print_help_list || print_version)
    {
      n_infiles = 0;

      /* Create a dummy input file, so that we can pass
	 the help option on to the various sub-processes.  */
      add_infile ("help-dummy", "c");
    }

  alloc_switch ();
  switches[n_switches].part1 = 0;
  alloc_infile ();
  infiles[n_infiles].name = 0;
}

/* Store switches not filtered out by %<S in spec in COLLECT_GCC_OPTIONS
   and place that in the environment.  */

static void
set_collect_gcc_options (void)
{
  int i;
  int first_time;

  /* Build COLLECT_GCC_OPTIONS to have all of the options specified to
     the compiler.  */
  obstack_grow (&collect_obstack, "COLLECT_GCC_OPTIONS=",
		sizeof ("COLLECT_GCC_OPTIONS=") - 1);

  first_time = TRUE;
  for (i = 0; (int) i < n_switches; i++)
    {
      const char *const *args;
      const char *p, *q;
      if (!first_time)
	obstack_grow (&collect_obstack, " ", 1);

      first_time = FALSE;

      /* Ignore elided switches.  */
      if ((switches[i].live_cond
	   & (SWITCH_IGNORE | SWITCH_KEEP_FOR_GCC))
	  == SWITCH_IGNORE)
	continue;

      obstack_grow (&collect_obstack, "'-", 2);
      q = switches[i].part1;
      while ((p = strchr (q, '\'')))
	{
	  obstack_grow (&collect_obstack, q, p - q);
	  obstack_grow (&collect_obstack, "'\\''", 4);
	  q = ++p;
	}
      obstack_grow (&collect_obstack, q, strlen (q));
      obstack_grow (&collect_obstack, "'", 1);

      for (args = switches[i].args; args && *args; args++)
	{
	  obstack_grow (&collect_obstack, " '", 2);
	  q = *args;
	  while ((p = strchr (q, '\'')))
	    {
	      obstack_grow (&collect_obstack, q, p - q);
	      obstack_grow (&collect_obstack, "'\\''", 4);
	      q = ++p;
	    }
	  obstack_grow (&collect_obstack, q, strlen (q));
	  obstack_grow (&collect_obstack, "'", 1);
	}
    }
  obstack_grow (&collect_obstack, "\0", 1);
  xputenv (XOBFINISH (&collect_obstack, char *));
}

/* Process a spec string, accumulating and running commands.  */

/* These variables describe the input file name.
   input_file_number is the index on outfiles of this file,
   so that the output file name can be stored for later use by %o.
   input_basename is the start of the part of the input file
   sans all directory names, and basename_length is the number
   of characters starting there excluding the suffix .c or whatever.  */

static const char *gcc_input_filename;
static int input_file_number;
size_t input_filename_length;
static int basename_length;
static int suffixed_basename_length;
static const char *input_basename;
static const char *input_suffix;
#ifndef HOST_LACKS_INODE_NUMBERS
static struct stat input_stat;
#endif
static int input_stat_set;

/* The compiler used to process the current input file.  */
static struct compiler *input_file_compiler;

/* These are variables used within do_spec and do_spec_1.  */

/* Nonzero if an arg has been started and not yet terminated
   (with space, tab or newline).  */
static int arg_going;

/* Nonzero means %d or %g has been seen; the next arg to be terminated
   is a temporary file name.  */
static int delete_this_arg;

/* Nonzero means %w has been seen; the next arg to be terminated
   is the output file name of this compilation.  */
static int this_is_output_file;

/* Nonzero means %s has been seen; the next arg to be terminated
   is the name of a library file and we should try the standard
   search dirs for it.  */
static int this_is_library_file;

/* Nonzero means %T has been seen; the next arg to be terminated
   is the name of a linker script and we should try all of the
   standard search dirs for it.  If it is found insert a --script
   command line switch and then substitute the full path in place,
   otherwise generate an error message.  */
static int this_is_linker_script;

/* Nonzero means that the input of this command is coming from a pipe.  */
static int input_from_pipe;

/* Nonnull means substitute this for any suffix when outputting a switches
   arguments.  */
static const char *suffix_subst;

/* If there is an argument being accumulated, terminate it and store it.  */

static void
end_going_arg (void)
{
  if (arg_going)
    {
      const char *string;

      obstack_1grow (&obstack, 0);
      string = XOBFINISH (&obstack, const char *);
      if (this_is_library_file)
	string = find_file (string);
      if (this_is_linker_script)
	{
	  char * full_script_path = find_a_file (&startfile_prefixes, string, R_OK, true);

	  if (full_script_path == NULL)
	    {
	      error ("unable to locate default linker script %qs in the library search paths", string);
	      /* Script was not found on search path.  */
	      return;
	    }
	  store_arg ("--script", false, false);
	  string = full_script_path;
	}
      store_arg (string, delete_this_arg, this_is_output_file);
      if (this_is_output_file)
	outfiles[input_file_number] = string;
      arg_going = 0;
    }
}


/* Parse the WRAPPER string which is a comma separated list of the command line
   and insert them into the beginning of argbuf.  */

static void
insert_wrapper (const char *wrapper)
{
  int n = 0;
  int i;
  char *buf = xstrdup (wrapper);
  char *p = buf;
  unsigned int old_length = argbuf.length ();

  do
    {
      n++;
      while (*p == ',')
        p++;
    }
  while ((p = strchr (p, ',')) != NULL);

  argbuf.safe_grow (old_length + n);
  memmove (argbuf.address () + n,
	   argbuf.address (),
	   old_length * sizeof (const_char_p));

  i = 0;
  p = buf;
  do
    {
      while (*p == ',')
        {
          *p = 0;
          p++;
        }
      argbuf[i] = p;
      i++;
    }
  while ((p = strchr (p, ',')) != NULL);
  gcc_assert (i == n);
}

/* Process the spec SPEC and run the commands specified therein.
   Returns 0 if the spec is successfully processed; -1 if failed.  */

int
do_spec (const char *spec)
{
  int value;

  value = do_spec_2 (spec);

  /* Force out any unfinished command.
     If -pipe, this forces out the last command if it ended in `|'.  */
  if (value == 0)
    {
      if (argbuf.length () > 0
	  && !strcmp (argbuf.last (), "|"))
	argbuf.pop ();

      set_collect_gcc_options ();

      if (argbuf.length () > 0)
	value = execute ();
    }

  return value;
}

static int
do_spec_2 (const char *spec)
{
  int result;

  clear_args ();
  arg_going = 0;
  delete_this_arg = 0;
  this_is_output_file = 0;
  this_is_library_file = 0;
  this_is_linker_script = 0;
  input_from_pipe = 0;
  suffix_subst = NULL;

  result = do_spec_1 (spec, 0, NULL);

  end_going_arg ();

  return result;
}


/* Process the given spec string and add any new options to the end
   of the switches/n_switches array.  */

static void
do_option_spec (const char *name, const char *spec)
{
  unsigned int i, value_count, value_len;
  const char *p, *q, *value;
  char *tmp_spec, *tmp_spec_p;

  if (configure_default_options[0].name == NULL)
    return;

  for (i = 0; i < ARRAY_SIZE (configure_default_options); i++)
    if (strcmp (configure_default_options[i].name, name) == 0)
      break;
  if (i == ARRAY_SIZE (configure_default_options))
    return;

  value = configure_default_options[i].value;
  value_len = strlen (value);

  /* Compute the size of the final spec.  */
  value_count = 0;
  p = spec;
  while ((p = strstr (p, "%(VALUE)")) != NULL)
    {
      p ++;
      value_count ++;
    }

  /* Replace each %(VALUE) by the specified value.  */
  tmp_spec = (char *) alloca (strlen (spec) + 1
		     + value_count * (value_len - strlen ("%(VALUE)")));
  tmp_spec_p = tmp_spec;
  q = spec;
  while ((p = strstr (q, "%(VALUE)")) != NULL)
    {
      memcpy (tmp_spec_p, q, p - q);
      tmp_spec_p = tmp_spec_p + (p - q);
      memcpy (tmp_spec_p, value, value_len);
      tmp_spec_p += value_len;
      q = p + strlen ("%(VALUE)");
    }
  strcpy (tmp_spec_p, q);

  do_self_spec (tmp_spec);
}

/* Process the given spec string and add any new options to the end
   of the switches/n_switches array.  */

static void
do_self_spec (const char *spec)
{
  int i;

  do_spec_2 (spec);
  do_spec_1 (" ", 0, NULL);

  /* Mark %<S switches processed by do_self_spec to be ignored permanently.
     do_self_specs adds the replacements to switches array, so it shouldn't
     be processed afterwards.  */
  for (i = 0; i < n_switches; i++)
    if ((switches[i].live_cond & SWITCH_IGNORE))
      switches[i].live_cond |= SWITCH_IGNORE_PERMANENTLY;

  if (argbuf.length () > 0)
    {
      const char **argbuf_copy;
      struct cl_decoded_option *decoded_options;
      struct cl_option_handlers handlers;
      unsigned int decoded_options_count;
      unsigned int j;

      /* Create a copy of argbuf with a dummy argv[0] entry for
	 decode_cmdline_options_to_array.  */
      argbuf_copy = XNEWVEC (const char *,
			     argbuf.length () + 1);
      argbuf_copy[0] = "";
      memcpy (argbuf_copy + 1, argbuf.address (),
	      argbuf.length () * sizeof (const char *));

      decode_cmdline_options_to_array (argbuf.length () + 1,
				       argbuf_copy,
				       CL_DRIVER, &decoded_options,
				       &decoded_options_count);
      free (argbuf_copy);

      set_option_handlers (&handlers);

      for (j = 1; j < decoded_options_count; j++)
	{
	  switch (decoded_options[j].opt_index)
	    {
	    case OPT_SPECIAL_input_file:
	      /* Specs should only generate options, not input
		 files.  */
	      if (strcmp (decoded_options[j].arg, "-") != 0)
		fatal_error ("switch %qs does not start with %<-%>",
			     decoded_options[j].arg);
	      else
		fatal_error ("spec-generated switch is just %<-%>");
	      break;

	    case OPT_fcompare_debug_second:
	    case OPT_fcompare_debug:
	    case OPT_fcompare_debug_:
	    case OPT_o:
	      /* Avoid duplicate processing of some options from
		 compare-debug specs; just save them here.  */
	      save_switch (decoded_options[j].canonical_option[0],
			   (decoded_options[j].canonical_option_num_elements
			    - 1),
			   &decoded_options[j].canonical_option[1], false, true);
	      break;

	    default:
	      read_cmdline_option (&global_options, &global_options_set,
				   decoded_options + j, UNKNOWN_LOCATION,
				   CL_DRIVER, &handlers, global_dc);
	      break;
	    }
	}

      alloc_switch ();
      switches[n_switches].part1 = 0;
    }
}

/* Callback for processing %D and %I specs.  */

struct spec_path_info {
  const char *option;
  const char *append;
  size_t append_len;
  bool omit_relative;
  bool separate_options;
};

static void *
spec_path (char *path, void *data)
{
  struct spec_path_info *info = (struct spec_path_info *) data;
  size_t len = 0;
  char save = 0;

  if (info->omit_relative && !IS_ABSOLUTE_PATH (path))
    return NULL;

  if (info->append_len != 0)
    {
      len = strlen (path);
      memcpy (path + len, info->append, info->append_len + 1);
    }

  if (!is_directory (path, true))
    return NULL;

  do_spec_1 (info->option, 1, NULL);
  if (info->separate_options)
    do_spec_1 (" ", 0, NULL);

  if (info->append_len == 0)
    {
      len = strlen (path);
      save = path[len - 1];
      if (IS_DIR_SEPARATOR (path[len - 1]))
	path[len - 1] = '\0';
    }

  do_spec_1 (path, 1, NULL);
  do_spec_1 (" ", 0, NULL);

  /* Must not damage the original path.  */
  if (info->append_len == 0)
    path[len - 1] = save;

  return NULL;
}

/* Create a temporary FILE with the contents of ARGV. Add @FILE to the
   argument list. */

static void
create_at_file (char **argv)
{
  char *temp_file = make_temp_file ("");
  char *at_argument = concat ("@", temp_file, NULL);
  FILE *f = fopen (temp_file, "w");
  int status;

  if (f == NULL)
    fatal_error ("could not open temporary response file %s",
		 temp_file);

  status = writeargv (argv, f);

  if (status)
    fatal_error ("could not write to temporary response file %s",
		 temp_file);

  status = fclose (f);

  if (EOF == status)
    fatal_error ("could not close temporary response file %s",
		 temp_file);

  store_arg (at_argument, 0, 0);

  record_temp_file (temp_file, !save_temps_flag, !save_temps_flag);
}

/* True if we should compile INFILE. */

static bool
compile_input_file_p (struct infile *infile)
{
  if ((!infile->language) || (infile->language[0] != '*'))
    if (infile->incompiler == input_file_compiler)
      return true;
  return false;
}

/* Process each member of VEC as a spec.  */

static void
do_specs_vec (vec<char_p> vec)
{
  unsigned ix;
  char *opt;

  FOR_EACH_VEC_ELT (vec, ix, opt)
    {
      do_spec_1 (opt, 1, NULL);
      /* Make each accumulated option a separate argument.  */
      do_spec_1 (" ", 0, NULL);
    }
}

/* Process the sub-spec SPEC as a portion of a larger spec.
   This is like processing a whole spec except that we do
   not initialize at the beginning and we do not supply a
   newline by default at the end.
   INSWITCH nonzero means don't process %-sequences in SPEC;
   in this case, % is treated as an ordinary character.
   This is used while substituting switches.
   INSWITCH nonzero also causes SPC not to terminate an argument.

   Value is zero unless a line was finished
   and the command on that line reported an error.  */

static int
do_spec_1 (const char *spec, int inswitch, const char *soft_matched_part)
{
  const char *p = spec;
  int c;
  int i;
  int value;

  /* If it's an empty string argument to a switch, keep it as is.  */
  if (inswitch && !*p)
    arg_going = 1;

  while ((c = *p++))
    /* If substituting a switch, treat all chars like letters.
       Otherwise, NL, SPC, TAB and % are special.  */
    switch (inswitch ? 'a' : c)
      {
      case '\n':
	end_going_arg ();

	if (argbuf.length () > 0
	    && !strcmp (argbuf.last (), "|"))
	  {
	    /* A `|' before the newline means use a pipe here,
	       but only if -pipe was specified.
	       Otherwise, execute now and don't pass the `|' as an arg.  */
	    if (use_pipes)
	      {
		input_from_pipe = 1;
		break;
	      }
	    else
	      argbuf.pop ();
	  }

	set_collect_gcc_options ();

	if (argbuf.length () > 0)
	  {
	    value = execute ();
	    if (value)
	      return value;
	  }
	/* Reinitialize for a new command, and for a new argument.  */
	clear_args ();
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
	this_is_linker_script = 0;
	input_from_pipe = 0;
	break;

      case '|':
	end_going_arg ();

	/* Use pipe */
	obstack_1grow (&obstack, c);
	arg_going = 1;
	break;

      case '\t':
      case ' ':
	end_going_arg ();

	/* Reinitialize for a new argument.  */
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
	this_is_linker_script = 0;
	break;

      case '%':
	switch (c = *p++)
	  {
	  case 0:
	    fatal_error ("spec %qs invalid", spec);

	  case 'b':
	    if (save_temps_length)
	      obstack_grow (&obstack, save_temps_prefix, save_temps_length);
	    else
	      obstack_grow (&obstack, input_basename, basename_length);
	    if (compare_debug < 0)
	      obstack_grow (&obstack, ".gk", 3);
	    arg_going = 1;
	    break;

	  case 'B':
	    if (save_temps_length)
	      obstack_grow (&obstack, save_temps_prefix, save_temps_length);
	    else
	      obstack_grow (&obstack, input_basename, suffixed_basename_length);
	    if (compare_debug < 0)
	      obstack_grow (&obstack, ".gk", 3);
	    arg_going = 1;
	    break;

	  case 'd':
	    delete_this_arg = 2;
	    break;

	  /* Dump out the directories specified with LIBRARY_PATH,
	     followed by the absolute directories
	     that we search for startfiles.  */
	  case 'D':
	    {
	      struct spec_path_info info;

	      info.option = "-L";
	      info.append_len = 0;
#ifdef RELATIVE_PREFIX_NOT_LINKDIR
	      /* Used on systems which record the specified -L dirs
		 and use them to search for dynamic linking.
		 Relative directories always come from -B,
		 and it is better not to use them for searching
		 at run time.  In particular, stage1 loses.  */
	      info.omit_relative = true;
#else
	      info.omit_relative = false;
#endif
	      info.separate_options = false;

	      for_each_path (&startfile_prefixes, true, 0, spec_path, &info);
	    }
	    break;

	  case 'e':
	    /* %efoo means report an error with `foo' as error message
	       and don't execute any more commands for this file.  */
	    {
	      const char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n')
		p++;
	      buf = (char *) alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      error ("%s", _(buf));
	      return -1;
	    }
	    break;
	  case 'n':
	    /* %nfoo means report a notice with `foo' on stderr.  */
	    {
	      const char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n')
		p++;
	      buf = (char *) alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      inform (0, "%s", _(buf));
	      if (*p)
		p++;
	    }
	    break;

	  case 'j':
	    {
	      struct stat st;

	      /* If save_temps_flag is off, and the HOST_BIT_BUCKET is
		 defined, and it is not a directory, and it is
		 writable, use it.  Otherwise, treat this like any
		 other temporary file.  */

	      if ((!save_temps_flag)
		  && (stat (HOST_BIT_BUCKET, &st) == 0) && (!S_ISDIR (st.st_mode))
		  && (access (HOST_BIT_BUCKET, W_OK) == 0))
		{
		  obstack_grow (&obstack, HOST_BIT_BUCKET,
				strlen (HOST_BIT_BUCKET));
		  delete_this_arg = 0;
		  arg_going = 1;
		  break;
		}
	    }
	    goto create_temp_file;
	  case '|':
	    if (use_pipes)
	      {
		obstack_1grow (&obstack, '-');
		delete_this_arg = 0;
		arg_going = 1;

		/* consume suffix */
		while (*p == '.' || ISALNUM ((unsigned char) *p))
		  p++;
		if (p[0] == '%' && p[1] == 'O')
		  p += 2;

		break;
	      }
	    goto create_temp_file;
	  case 'm':
	    if (use_pipes)
	      {
		/* consume suffix */
		while (*p == '.' || ISALNUM ((unsigned char) *p))
		  p++;
		if (p[0] == '%' && p[1] == 'O')
		  p += 2;

		break;
	      }
	    goto create_temp_file;
	  case 'g':
	  case 'u':
	  case 'U':
	  create_temp_file:
	      {
		struct temp_name *t;
		int suffix_length;
		const char *suffix = p;
		char *saved_suffix = NULL;

		while (*p == '.' || ISALNUM ((unsigned char) *p))
		  p++;
		suffix_length = p - suffix;
		if (p[0] == '%' && p[1] == 'O')
		  {
		    p += 2;
		    /* We don't support extra suffix characters after %O.  */
		    if (*p == '.' || ISALNUM ((unsigned char) *p))
		      fatal_error ("spec %qs has invalid %<%%0%c%>", spec, *p);
		    if (suffix_length == 0)
		      suffix = TARGET_OBJECT_SUFFIX;
		    else
		      {
			saved_suffix
			  = XNEWVEC (char, suffix_length
				     + strlen (TARGET_OBJECT_SUFFIX));
			strncpy (saved_suffix, suffix, suffix_length);
			strcpy (saved_suffix + suffix_length,
				TARGET_OBJECT_SUFFIX);
		      }
		    suffix_length += strlen (TARGET_OBJECT_SUFFIX);
		  }

		if (compare_debug < 0)
		  {
		    suffix = concat (".gk", suffix, NULL);
		    suffix_length += 3;
		  }

		/* If -save-temps=obj and -o were specified, use that for the
		   temp file.  */
		if (save_temps_length)
		  {
		    char *tmp;
		    temp_filename_length
		      = save_temps_length + suffix_length + 1;
		    tmp = (char *) alloca (temp_filename_length);
		    memcpy (tmp, save_temps_prefix, save_temps_length);
		    memcpy (tmp + save_temps_length, suffix, suffix_length);
		    tmp[save_temps_length + suffix_length] = '\0';
		    temp_filename = save_string (tmp, save_temps_length
						      + suffix_length);
		    obstack_grow (&obstack, temp_filename,
				  temp_filename_length);
		    arg_going = 1;
		    delete_this_arg = 0;
		    break;
		  }

		/* If the gcc_input_filename has the same suffix specified
		   for the %g, %u, or %U, and -save-temps is specified,
		   we could end up using that file as an intermediate
		   thus clobbering the user's source file (.e.g.,
		   gcc -save-temps foo.s would clobber foo.s with the
		   output of cpp0).  So check for this condition and
		   generate a temp file as the intermediate.  */

		if (save_temps_flag)
		  {
		    char *tmp;
		    temp_filename_length = basename_length + suffix_length + 1;
		    tmp = (char *) alloca (temp_filename_length);
		    memcpy (tmp, input_basename, basename_length);
		    memcpy (tmp + basename_length, suffix, suffix_length);
		    tmp[basename_length + suffix_length] = '\0';
		    temp_filename = tmp;

		    if (filename_cmp (temp_filename, gcc_input_filename) != 0)
		      {
#ifndef HOST_LACKS_INODE_NUMBERS
			struct stat st_temp;

			/* Note, set_input() resets input_stat_set to 0.  */
			if (input_stat_set == 0)
			  {
			    input_stat_set = stat (gcc_input_filename,
						   &input_stat);
			    if (input_stat_set >= 0)
			      input_stat_set = 1;
			  }

			/* If we have the stat for the gcc_input_filename
			   and we can do the stat for the temp_filename
			   then the they could still refer to the same
			   file if st_dev/st_ino's are the same.  */
			if (input_stat_set != 1
			    || stat (temp_filename, &st_temp) < 0
			    || input_stat.st_dev != st_temp.st_dev
			    || input_stat.st_ino != st_temp.st_ino)
#else
			/* Just compare canonical pathnames.  */
			char* input_realname = lrealpath (gcc_input_filename);
			char* temp_realname = lrealpath (temp_filename);
			bool files_differ = filename_cmp (input_realname, temp_realname);
			free (input_realname);
			free (temp_realname);
			if (files_differ)
#endif
			  {
			    temp_filename = save_string (temp_filename,
							 temp_filename_length + 1);
			    obstack_grow (&obstack, temp_filename,
						    temp_filename_length);
			    arg_going = 1;
			    delete_this_arg = 0;
			    break;
			  }
		      }
		  }

		/* See if we already have an association of %g/%u/%U and
		   suffix.  */
		for (t = temp_names; t; t = t->next)
		  if (t->length == suffix_length
		      && strncmp (t->suffix, suffix, suffix_length) == 0
		      && t->unique == (c == 'u' || c == 'U' || c == 'j'))
		    break;

		/* Make a new association if needed.  %u and %j
		   require one.  */
		if (t == 0 || c == 'u' || c == 'j')
		  {
		    if (t == 0)
		      {
			t = XNEW (struct temp_name);
			t->next = temp_names;
			temp_names = t;
		      }
		    t->length = suffix_length;
		    if (saved_suffix)
		      {
			t->suffix = saved_suffix;
			saved_suffix = NULL;
		      }
		    else
		      t->suffix = save_string (suffix, suffix_length);
		    t->unique = (c == 'u' || c == 'U' || c == 'j');
		    temp_filename = make_temp_file (t->suffix);
		    temp_filename_length = strlen (temp_filename);
		    t->filename = temp_filename;
		    t->filename_length = temp_filename_length;
		  }

		free (saved_suffix);

		obstack_grow (&obstack, t->filename, t->filename_length);
		delete_this_arg = 1;
	      }
	    arg_going = 1;
	    break;

	  case 'i':
	    if (combine_inputs)
	      {
		if (at_file_supplied)
		  {
		    /* We are going to expand `%i' to `@FILE', where FILE
		       is a newly-created temporary filename.  The filenames
		       that would usually be expanded in place of %o will be
		       written to the temporary file.  */
		    char **argv;
		    int n_files = 0;
		    int j;

		    for (i = 0; i < n_infiles; i++)
		      if (compile_input_file_p (&infiles[i]))
			n_files++;

		    argv = (char **) alloca (sizeof (char *) * (n_files + 1));

		    /* Copy the strings over.  */
		    for (i = 0, j = 0; i < n_infiles; i++)
		      if (compile_input_file_p (&infiles[i]))
			{
			  argv[j] = CONST_CAST (char *, infiles[i].name);
			  infiles[i].compiled = true;
			  j++;
			}
		    argv[j] = NULL;

		    create_at_file (argv);
		  }
		else
		  for (i = 0; (int) i < n_infiles; i++)
		    if (compile_input_file_p (&infiles[i]))
		      {
			store_arg (infiles[i].name, 0, 0);
			infiles[i].compiled = true;
		      }
	      }
	    else
	      {
		obstack_grow (&obstack, gcc_input_filename,
			      input_filename_length);
		arg_going = 1;
	      }
	    break;

	  case 'I':
	    {
	      struct spec_path_info info;

	      if (multilib_dir)
		{
		  do_spec_1 ("-imultilib", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (multilib_dir, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      if (multiarch_dir)
		{
		  do_spec_1 ("-imultiarch", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (multiarch_dir, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      if (gcc_exec_prefix)
		{
		  do_spec_1 ("-iprefix", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (gcc_exec_prefix, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      if (target_system_root_changed ||
		  (target_system_root && target_sysroot_hdrs_suffix))
		{
		  do_spec_1 ("-isysroot", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (target_system_root, 1, NULL);
		  if (target_sysroot_hdrs_suffix)
		    do_spec_1 (target_sysroot_hdrs_suffix, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      info.option = "-isystem";
	      info.append = "include";
	      info.append_len = strlen (info.append);
	      info.omit_relative = false;
	      info.separate_options = true;

	      for_each_path (&include_prefixes, false, info.append_len,
			     spec_path, &info);

	      info.append = "include-fixed";
	      if (*sysroot_hdrs_suffix_spec)
		info.append = concat (info.append, dir_separator_str,
				      multilib_dir, NULL);
	      info.append_len = strlen (info.append);
	      for_each_path (&include_prefixes, false, info.append_len,
			     spec_path, &info);
	    }
	    break;

	  case 'o':
	    {
	      int max = n_infiles;
	      max += lang_specific_extra_outfiles;

              if (HAVE_GNU_LD && at_file_supplied)
                {
                  /* We are going to expand `%o' to `@FILE', where FILE
                     is a newly-created temporary filename.  The filenames
                     that would usually be expanded in place of %o will be
                     written to the temporary file.  */

                  char **argv;
                  int n_files, j;

                  /* Convert OUTFILES into a form suitable for writeargv.  */

                  /* Determine how many are non-NULL.  */
                  for (n_files = 0, i = 0; i < max; i++)
                    n_files += outfiles[i] != NULL;

                  argv = (char **) alloca (sizeof (char *) * (n_files + 1));

                  /* Copy the strings over.  */
                  for (i = 0, j = 0; i < max; i++)
                    if (outfiles[i])
                      {
                        argv[j] = CONST_CAST (char *, outfiles[i]);
                        j++;
                      }
                  argv[j] = NULL;

		  create_at_file (argv);
                }
              else
                for (i = 0; i < max; i++)
	          if (outfiles[i])
		    store_arg (outfiles[i], 0, 0);
	      break;
	    }

	  case 'O':
	    obstack_grow (&obstack, TARGET_OBJECT_SUFFIX, strlen (TARGET_OBJECT_SUFFIX));
	    arg_going = 1;
	    break;

	  case 's':
	    this_is_library_file = 1;
	    break;

	  case 'T':
	    this_is_linker_script = 1;
	    break;

	  case 'V':
	    outfiles[input_file_number] = NULL;
	    break;

	  case 'w':
	    this_is_output_file = 1;
	    break;

	  case 'W':
	    {
	      unsigned int cur_index = argbuf.length ();
	      /* Handle the {...} following the %W.  */
	      if (*p != '{')
		fatal_error ("spec %qs has invalid %<%%W%c%>", spec, *p);
	      p = handle_braces (p + 1);
	      if (p == 0)
		return -1;
	      end_going_arg ();
	      /* If any args were output, mark the last one for deletion
		 on failure.  */
	      if (argbuf.length () != cur_index)
		record_temp_file (argbuf.last (), 0, 1);
	      break;
	    }

	  /* %x{OPTION} records OPTION for %X to output.  */
	  case 'x':
	    {
	      const char *p1 = p;
	      char *string;
	      char *opt;
	      unsigned ix;

	      /* Skip past the option value and make a copy.  */
	      if (*p != '{')
		fatal_error ("spec %qs has invalid %<%%x%c%>", spec, *p);
	      while (*p++ != '}')
		;
	      string = save_string (p1 + 1, p - p1 - 2);

	      /* See if we already recorded this option.  */
	      FOR_EACH_VEC_ELT (linker_options, ix, opt)
		if (! strcmp (string, opt))
		  {
		    free (string);
		    return 0;
		  }

	      /* This option is new; add it.  */
	      add_linker_option (string, strlen (string));
	      free (string);
	    }
	    break;

	  /* Dump out the options accumulated previously using %x.  */
	  case 'X':
	    do_specs_vec (linker_options);
	    break;

	  /* Dump out the options accumulated previously using -Wa,.  */
	  case 'Y':
	    do_specs_vec (assembler_options);
	    break;

	  /* Dump out the options accumulated previously using -Wp,.  */
	  case 'Z':
	    do_specs_vec (preprocessor_options);
	    break;

	    /* Here are digits and numbers that just process
	       a certain constant string as a spec.  */

	  case '1':
	    value = do_spec_1 (cc1_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case '2':
	    value = do_spec_1 (cc1plus_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'a':
	    value = do_spec_1 (asm_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'A':
	    value = do_spec_1 (asm_final_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'C':
	    {
	      const char *const spec
		= (input_file_compiler->cpp_spec
		   ? input_file_compiler->cpp_spec
		   : cpp_spec);
	      value = do_spec_1 (spec, 0, NULL);
	      if (value != 0)
		return value;
	    }
	    break;

	  case 'E':
	    value = do_spec_1 (endfile_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'l':
	    value = do_spec_1 (link_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'L':
	    value = do_spec_1 (lib_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'M':
	    if (multilib_os_dir == NULL)
	      obstack_1grow (&obstack, '.');
	    else
	      obstack_grow (&obstack, multilib_os_dir,
			    strlen (multilib_os_dir));
	    break;

	  case 'G':
	    value = do_spec_1 (libgcc_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'R':
	    /* We assume there is a directory
	       separator at the end of this string.  */
	    if (target_system_root)
	      {
	        obstack_grow (&obstack, target_system_root,
			      strlen (target_system_root));
		if (target_sysroot_suffix)
		  obstack_grow (&obstack, target_sysroot_suffix,
				strlen (target_sysroot_suffix));
	      }
	    break;

	  case 'S':
	    value = do_spec_1 (startfile_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	    /* Here we define characters other than letters and digits.  */

	  case '{':
	    p = handle_braces (p);
	    if (p == 0)
	      return -1;
	    break;

	  case ':':
	    p = handle_spec_function (p);
	    if (p == 0)
	      return -1;
	    break;

	  case '%':
	    obstack_1grow (&obstack, '%');
	    break;

	  case '.':
	    {
	      unsigned len = 0;

	      while (p[len] && p[len] != ' ' && p[len] != '%')
		len++;
	      suffix_subst = save_string (p - 1, len + 1);
	      p += len;
	    }
	   break;

	   /* Henceforth ignore the option(s) matching the pattern
	      after the %<.  */
	  case '<':
	  case '>':
	    {
	      unsigned len = 0;
	      int have_wildcard = 0;
	      int i;
	      int switch_option;

	      if (c == '>')
		switch_option = SWITCH_IGNORE | SWITCH_KEEP_FOR_GCC;
	      else
		switch_option = SWITCH_IGNORE;

	      while (p[len] && p[len] != ' ' && p[len] != '\t')
		len++;

	      if (p[len-1] == '*')
		have_wildcard = 1;

	      for (i = 0; i < n_switches; i++)
		if (!strncmp (switches[i].part1, p, len - have_wildcard)
		    && (have_wildcard || switches[i].part1[len] == '\0'))
		  {
		    switches[i].live_cond |= switch_option;
		    /* User switch be validated from validate_all_switches.
		       when the definition is seen from the spec file.
		       If not defined anywhere, will be rejected.  */
		    if (switches[i].known)
		      switches[i].validated = true;
		  }

	      p += len;
	    }
	    break;

	  case '*':
	    if (soft_matched_part)
	      {
		if (soft_matched_part[0])
		  do_spec_1 (soft_matched_part, 1, NULL);
		do_spec_1 (" ", 0, NULL);
	      }
	    else
	      /* Catch the case where a spec string contains something like
		 '%{foo:%*}'.  i.e. there is no * in the pattern on the left
		 hand side of the :.  */
	      error ("spec failure: %<%%*%> has not been initialized by pattern match");
	    break;

	    /* Process a string found as the value of a spec given by name.
	       This feature allows individual machine descriptions
	       to add and use their own specs.  */
	  case '(':
	    {
	      const char *name = p;
	      struct spec_list *sl;
	      int len;

	      /* The string after the S/P is the name of a spec that is to be
		 processed.  */
	      while (*p && *p != ')')
		p++;

	      /* See if it's in the list.  */
	      for (len = p - name, sl = specs; sl; sl = sl->next)
		if (sl->name_len == len && !strncmp (sl->name, name, len))
		  {
		    name = *(sl->ptr_spec);
#ifdef DEBUG_SPECS
		    fnotice (stderr, "Processing spec (%s), which is '%s'\n",
			     sl->name, name);
#endif
		    break;
		  }

	      if (sl)
		{
		  value = do_spec_1 (name, 0, NULL);
		  if (value != 0)
		    return value;
		}

	      /* Discard the closing paren.  */
	      if (*p)
		p++;
	    }
	    break;

	  default:
	    error ("spec failure: unrecognized spec option %qc", c);
	    break;
	  }
	break;

      case '\\':
	/* Backslash: treat next character as ordinary.  */
	c = *p++;

	/* Fall through.  */
      default:
	/* Ordinary character: put it into the current argument.  */
	obstack_1grow (&obstack, c);
	arg_going = 1;
      }

  /* End of string.  If we are processing a spec function, we need to
     end any pending argument.  */
  if (processing_spec_function)
    end_going_arg ();

  return 0;
}

/* Look up a spec function.  */

static const struct spec_function *
lookup_spec_function (const char *name)
{
  const struct spec_function *sf;

  for (sf = static_spec_functions; sf->name != NULL; sf++)
    if (strcmp (sf->name, name) == 0)
      return sf;

  return NULL;
}

/* Evaluate a spec function.  */

static const char *
eval_spec_function (const char *func, const char *args)
{
  const struct spec_function *sf;
  const char *funcval;

  /* Saved spec processing context.  */
  vec<const_char_p> save_argbuf;

  int save_arg_going;
  int save_delete_this_arg;
  int save_this_is_output_file;
  int save_this_is_library_file;
  int save_input_from_pipe;
  int save_this_is_linker_script;
  const char *save_suffix_subst;

  int save_growing_size;
  void *save_growing_value;

  sf = lookup_spec_function (func);
  if (sf == NULL)
    fatal_error ("unknown spec function %qs", func);

  /* Push the spec processing context.  */
  save_argbuf = argbuf;

  save_arg_going = arg_going;
  save_delete_this_arg = delete_this_arg;
  save_this_is_output_file = this_is_output_file;
  save_this_is_library_file = this_is_library_file;
  save_this_is_linker_script = this_is_linker_script;
  save_input_from_pipe = input_from_pipe;
  save_suffix_subst = suffix_subst;

  /* If we have some object growing now, finalize it so the args and function
     eval proceed from a cleared context.  This is needed to prevent the first
     constructed arg from mistakenly including the growing value.  We'll push
     this value back on the obstack once the function evaluation is done, to
     restore a consistent processing context for our caller.  This is fine as
     the address of growing objects isn't guaranteed to remain stable until
     they are finalized, and we expect this situation to be rare enough for
     the extra copy not to be an issue.  */
  save_growing_size = obstack_object_size (&obstack);
  if (save_growing_size > 0)
    save_growing_value = obstack_finish (&obstack);

  /* Create a new spec processing context, and build the function
     arguments.  */

  alloc_args ();
  if (do_spec_2 (args) < 0)
    fatal_error ("error in args to spec function %qs", func);

  /* argbuf_index is an index for the next argument to be inserted, and
     so contains the count of the args already inserted.  */

  funcval = (*sf->func) (argbuf.length (),
			 argbuf.address ());

  /* Pop the spec processing context.  */
  argbuf.release ();
  argbuf = save_argbuf;

  arg_going = save_arg_going;
  delete_this_arg = save_delete_this_arg;
  this_is_output_file = save_this_is_output_file;
  this_is_library_file = save_this_is_library_file;
  this_is_linker_script = save_this_is_linker_script;
  input_from_pipe = save_input_from_pipe;
  suffix_subst = save_suffix_subst;

  if (save_growing_size > 0)
    obstack_grow (&obstack, save_growing_value, save_growing_size);

  return funcval;
}

/* Handle a spec function call of the form:

   %:function(args)

   ARGS is processed as a spec in a separate context and split into an
   argument vector in the normal fashion.  The function returns a string
   containing a spec which we then process in the caller's context, or
   NULL if no processing is required.  */

static const char *
handle_spec_function (const char *p)
{
  char *func, *args;
  const char *endp, *funcval;
  int count;

  processing_spec_function++;

  /* Get the function name.  */
  for (endp = p; *endp != '\0'; endp++)
    {
      if (*endp == '(')		/* ) */
        break;
      /* Only allow [A-Za-z0-9], -, and _ in function names.  */
      if (!ISALNUM (*endp) && !(*endp == '-' || *endp == '_'))
	fatal_error ("malformed spec function name");
    }
  if (*endp != '(')		/* ) */
    fatal_error ("no arguments for spec function");
  func = save_string (p, endp - p);
  p = ++endp;

  /* Get the arguments.  */
  for (count = 0; *endp != '\0'; endp++)
    {
      /* ( */
      if (*endp == ')')
	{
	  if (count == 0)
	    break;
	  count--;
	}
      else if (*endp == '(')	/* ) */
	count++;
    }
  /* ( */
  if (*endp != ')')
    fatal_error ("malformed spec function arguments");
  args = save_string (p, endp - p);
  p = ++endp;

  /* p now points to just past the end of the spec function expression.  */

  funcval = eval_spec_function (func, args);
  if (funcval != NULL && do_spec_1 (funcval, 0, NULL) < 0)
    p = NULL;

  free (func);
  free (args);

  processing_spec_function--;

  return p;
}

/* Inline subroutine of handle_braces.  Returns true if the current
   input suffix matches the atom bracketed by ATOM and END_ATOM.  */
static inline bool
input_suffix_matches (const char *atom, const char *end_atom)
{
  return (input_suffix
	  && !strncmp (input_suffix, atom, end_atom - atom)
	  && input_suffix[end_atom - atom] == '\0');
}

/* Subroutine of handle_braces.  Returns true if the current
   input file's spec name matches the atom bracketed by ATOM and END_ATOM.  */
static bool
input_spec_matches (const char *atom, const char *end_atom)
{
  return (input_file_compiler
	  && input_file_compiler->suffix
	  && input_file_compiler->suffix[0] != '\0'
	  && !strncmp (input_file_compiler->suffix + 1, atom,
		       end_atom - atom)
	  && input_file_compiler->suffix[end_atom - atom + 1] == '\0');
}

/* Subroutine of handle_braces.  Returns true if a switch
   matching the atom bracketed by ATOM and END_ATOM appeared on the
   command line.  */
static bool
switch_matches (const char *atom, const char *end_atom, int starred)
{
  int i;
  int len = end_atom - atom;
  int plen = starred ? len : -1;

  for (i = 0; i < n_switches; i++)
    if (!strncmp (switches[i].part1, atom, len)
	&& (starred || switches[i].part1[len] == '\0')
	&& check_live_switch (i, plen))
      return true;

    /* Check if a switch with separated form matching the atom.
       We check -D and -U switches. */
    else if (switches[i].args != 0)
      {
	if ((*switches[i].part1 == 'D' || *switches[i].part1 == 'U')
	    && *switches[i].part1 == atom[0])
	  {
	    if (!strncmp (switches[i].args[0], &atom[1], len - 1)
		&& (starred || (switches[i].part1[1] == '\0'
				&& switches[i].args[0][len - 1] == '\0'))
		&& check_live_switch (i, (starred ? 1 : -1)))
	      return true;
	  }
      }

  return false;
}

/* Inline subroutine of handle_braces.  Mark all of the switches which
   match ATOM (extends to END_ATOM; STARRED indicates whether there
   was a star after the atom) for later processing.  */
static inline void
mark_matching_switches (const char *atom, const char *end_atom, int starred)
{
  int i;
  int len = end_atom - atom;
  int plen = starred ? len : -1;

  for (i = 0; i < n_switches; i++)
    if (!strncmp (switches[i].part1, atom, len)
	&& (starred || switches[i].part1[len] == '\0')
	&& check_live_switch (i, plen))
      switches[i].ordering = 1;
}

/* Inline subroutine of handle_braces.  Process all the currently
   marked switches through give_switch, and clear the marks.  */
static inline void
process_marked_switches (void)
{
  int i;

  for (i = 0; i < n_switches; i++)
    if (switches[i].ordering == 1)
      {
	switches[i].ordering = 0;
	give_switch (i, 0);
      }
}

/* Handle a %{ ... } construct.  P points just inside the leading {.
   Returns a pointer one past the end of the brace block, or 0
   if we call do_spec_1 and that returns -1.  */

static const char *
handle_braces (const char *p)
{
  const char *atom, *end_atom;
  const char *d_atom = NULL, *d_end_atom = NULL;
  const char *orig = p;

  bool a_is_suffix;
  bool a_is_spectype;
  bool a_is_starred;
  bool a_is_negated;
  bool a_matched;

  bool a_must_be_last = false;
  bool ordered_set    = false;
  bool disjunct_set   = false;
  bool disj_matched   = false;
  bool disj_starred   = true;
  bool n_way_choice   = false;
  bool n_way_matched  = false;

#define SKIP_WHITE() do { while (*p == ' ' || *p == '\t') p++; } while (0)

  do
    {
      if (a_must_be_last)
	goto invalid;

      /* Scan one "atom" (S in the description above of %{}, possibly
	 with '!', '.', '@', ',', or '*' modifiers).  */
      a_matched = false;
      a_is_suffix = false;
      a_is_starred = false;
      a_is_negated = false;
      a_is_spectype = false;

      SKIP_WHITE();
      if (*p == '!')
	p++, a_is_negated = true;

      SKIP_WHITE();
      if (*p == '.')
	p++, a_is_suffix = true;
      else if (*p == ',')
	p++, a_is_spectype = true;

      atom = p;
      while (ISIDNUM(*p) || *p == '-' || *p == '+' || *p == '='
	     || *p == ',' || *p == '.' || *p == '@')
	p++;
      end_atom = p;

      if (*p == '*')
	p++, a_is_starred = 1;

      SKIP_WHITE();
      switch (*p)
	{
	case '&': case '}':
	  /* Substitute the switch(es) indicated by the current atom.  */
	  ordered_set = true;
	  if (disjunct_set || n_way_choice || a_is_negated || a_is_suffix
	      || a_is_spectype || atom == end_atom)
	    goto invalid;

	  mark_matching_switches (atom, end_atom, a_is_starred);

	  if (*p == '}')
	    process_marked_switches ();
	  break;

	case '|': case ':':
	  /* Substitute some text if the current atom appears as a switch
	     or suffix.  */
	  disjunct_set = true;
	  if (ordered_set)
	    goto invalid;

	  if (atom == end_atom)
	    {
	      if (!n_way_choice || disj_matched || *p == '|'
		  || a_is_negated || a_is_suffix || a_is_spectype
		  || a_is_starred)
		goto invalid;

	      /* An empty term may appear as the last choice of an
		 N-way choice set; it means "otherwise".  */
	      a_must_be_last = true;
	      disj_matched = !n_way_matched;
	      disj_starred = false;
	    }
	  else
	    {
	      if ((a_is_suffix || a_is_spectype) && a_is_starred)
		goto invalid;

	      if (!a_is_starred)
		disj_starred = false;

	      /* Don't bother testing this atom if we already have a
		 match.  */
	      if (!disj_matched && !n_way_matched)
		{
		  if (a_is_suffix)
		    a_matched = input_suffix_matches (atom, end_atom);
		  else if (a_is_spectype)
		    a_matched = input_spec_matches (atom, end_atom);
		  else
		    a_matched = switch_matches (atom, end_atom, a_is_starred);

		  if (a_matched != a_is_negated)
		    {
		      disj_matched = true;
		      d_atom = atom;
		      d_end_atom = end_atom;
		    }
		}
	    }

	  if (*p == ':')
	    {
	      /* Found the body, that is, the text to substitute if the
		 current disjunction matches.  */
	      p = process_brace_body (p + 1, d_atom, d_end_atom, disj_starred,
				      disj_matched && !n_way_matched);
	      if (p == 0)
		return 0;

	      /* If we have an N-way choice, reset state for the next
		 disjunction.  */
	      if (*p == ';')
		{
		  n_way_choice = true;
		  n_way_matched |= disj_matched;
		  disj_matched = false;
		  disj_starred = true;
		  d_atom = d_end_atom = NULL;
		}
	    }
	  break;

	default:
	  goto invalid;
	}
    }
  while (*p++ != '}');

  return p;

 invalid:
  fatal_error ("braced spec %qs is invalid at %qc", orig, *p);

#undef SKIP_WHITE
}

/* Subroutine of handle_braces.  Scan and process a brace substitution body
   (X in the description of %{} syntax).  P points one past the colon;
   ATOM and END_ATOM bracket the first atom which was found to be true
   (present) in the current disjunction; STARRED indicates whether all
   the atoms in the current disjunction were starred (for syntax validation);
   MATCHED indicates whether the disjunction matched or not, and therefore
   whether or not the body is to be processed through do_spec_1 or just
   skipped.  Returns a pointer to the closing } or ;, or 0 if do_spec_1
   returns -1.  */

static const char *
process_brace_body (const char *p, const char *atom, const char *end_atom,
		    int starred, int matched)
{
  const char *body, *end_body;
  unsigned int nesting_level;
  bool have_subst     = false;

  /* Locate the closing } or ;, honoring nested braces.
     Trim trailing whitespace.  */
  body = p;
  nesting_level = 1;
  for (;;)
    {
      if (*p == '{')
	nesting_level++;
      else if (*p == '}')
	{
	  if (!--nesting_level)
	    break;
	}
      else if (*p == ';' && nesting_level == 1)
	break;
      else if (*p == '%' && p[1] == '*' && nesting_level == 1)
	have_subst = true;
      else if (*p == '\0')
	goto invalid;
      p++;
    }

  end_body = p;
  while (end_body[-1] == ' ' || end_body[-1] == '\t')
    end_body--;

  if (have_subst && !starred)
    goto invalid;

  if (matched)
    {
      /* Copy the substitution body to permanent storage and execute it.
	 If have_subst is false, this is a simple matter of running the
	 body through do_spec_1...  */
      char *string = save_string (body, end_body - body);
      if (!have_subst)
	{
	  if (do_spec_1 (string, 0, NULL) < 0)
	    return 0;
	}
      else
	{
	  /* ... but if have_subst is true, we have to process the
	     body once for each matching switch, with %* set to the
	     variant part of the switch.  */
	  unsigned int hard_match_len = end_atom - atom;
	  int i;

	  for (i = 0; i < n_switches; i++)
	    if (!strncmp (switches[i].part1, atom, hard_match_len)
		&& check_live_switch (i, hard_match_len))
	      {
		if (do_spec_1 (string, 0,
			       &switches[i].part1[hard_match_len]) < 0)
		  return 0;
		/* Pass any arguments this switch has.  */
		give_switch (i, 1);
		suffix_subst = NULL;
	      }
	}
    }

  return p;

 invalid:
  fatal_error ("braced spec body %qs is invalid", body);
}

/* Return 0 iff switch number SWITCHNUM is obsoleted by a later switch
   on the command line.  PREFIX_LENGTH is the length of XXX in an {XXX*}
   spec, or -1 if either exact match or %* is used.

   A -O switch is obsoleted by a later -O switch.  A -f, -g, -m, or -W switch
   whose value does not begin with "no-" is obsoleted by the same value
   with the "no-", similarly for a switch with the "no-" prefix.  */

static int
check_live_switch (int switchnum, int prefix_length)
{
  const char *name = switches[switchnum].part1;
  int i;

  /* If we already processed this switch and determined if it was
     live or not, return our past determination.  */
  if (switches[switchnum].live_cond != 0)
    return ((switches[switchnum].live_cond & SWITCH_LIVE) != 0
	    && (switches[switchnum].live_cond & SWITCH_FALSE) == 0
	    && (switches[switchnum].live_cond & SWITCH_IGNORE_PERMANENTLY)
	       == 0);

  /* In the common case of {<at-most-one-letter>*}, a negating
     switch would always match, so ignore that case.  We will just
     send the conflicting switches to the compiler phase.  */
  if (prefix_length >= 0 && prefix_length <= 1)
    return 1;

  /* Now search for duplicate in a manner that depends on the name.  */
  switch (*name)
    {
    case 'O':
      for (i = switchnum + 1; i < n_switches; i++)
	if (switches[i].part1[0] == 'O')
	  {
	    switches[switchnum].validated = true;
	    switches[switchnum].live_cond = SWITCH_FALSE;
	    return 0;
	  }
      break;

    case 'W':  case 'f':  case 'm': case 'g':
      if (! strncmp (name + 1, "no-", 3))
	{
	  /* We have Xno-YYY, search for XYYY.  */
	  for (i = switchnum + 1; i < n_switches; i++)
	    if (switches[i].part1[0] == name[0]
		&& ! strcmp (&switches[i].part1[1], &name[4]))
	      {
		/* --specs are validated with the validate_switches mechanism.  */
		if (switches[switchnum].known)
		  switches[switchnum].validated = true;
		switches[switchnum].live_cond = SWITCH_FALSE;
		return 0;
	      }
	}
      else
	{
	  /* We have XYYY, search for Xno-YYY.  */
	  for (i = switchnum + 1; i < n_switches; i++)
	    if (switches[i].part1[0] == name[0]
		&& switches[i].part1[1] == 'n'
		&& switches[i].part1[2] == 'o'
		&& switches[i].part1[3] == '-'
		&& !strcmp (&switches[i].part1[4], &name[1]))
	      {
		/* --specs are validated with the validate_switches mechanism.  */
		if (switches[switchnum].known)
		  switches[switchnum].validated = true;
		switches[switchnum].live_cond = SWITCH_FALSE;
		return 0;
	      }
	}
      break;
    }

  /* Otherwise the switch is live.  */
  switches[switchnum].live_cond |= SWITCH_LIVE;
  return 1;
}

/* Pass a switch to the current accumulating command
   in the same form that we received it.
   SWITCHNUM identifies the switch; it is an index into
   the vector of switches gcc received, which is `switches'.
   This cannot fail since it never finishes a command line.

   If OMIT_FIRST_WORD is nonzero, then we omit .part1 of the argument.  */

static void
give_switch (int switchnum, int omit_first_word)
{
  if ((switches[switchnum].live_cond & SWITCH_IGNORE) != 0)
    return;

  if (!omit_first_word)
    {
      do_spec_1 ("-", 0, NULL);
      do_spec_1 (switches[switchnum].part1, 1, NULL);
    }

  if (switches[switchnum].args != 0)
    {
      const char **p;
      for (p = switches[switchnum].args; *p; p++)
	{
	  const char *arg = *p;

	  do_spec_1 (" ", 0, NULL);
	  if (suffix_subst)
	    {
	      unsigned length = strlen (arg);
	      int dot = 0;

	      while (length-- && !IS_DIR_SEPARATOR (arg[length]))
		if (arg[length] == '.')
		  {
		    (CONST_CAST(char *, arg))[length] = 0;
		    dot = 1;
		    break;
		  }
	      do_spec_1 (arg, 1, NULL);
	      if (dot)
		(CONST_CAST(char *, arg))[length] = '.';
	      do_spec_1 (suffix_subst, 1, NULL);
	    }
	  else
	    do_spec_1 (arg, 1, NULL);
	}
    }

  do_spec_1 (" ", 0, NULL);
  switches[switchnum].validated = true;
}

/* Search for a file named NAME trying various prefixes including the
   user's -B prefix and some standard ones.
   Return the absolute file name found.  If nothing is found, return NAME.  */

static const char *
find_file (const char *name)
{
  char *newname = find_a_file (&startfile_prefixes, name, R_OK, true);
  return newname ? newname : name;
}

/* Determine whether a directory exists.  If LINKER, return 0 for
   certain fixed names not needed by the linker.  */

static int
is_directory (const char *path1, bool linker)
{
  int len1;
  char *path;
  char *cp;
  struct stat st;

  /* Ensure the string ends with "/.".  The resulting path will be a
     directory even if the given path is a symbolic link.  */
  len1 = strlen (path1);
  path = (char *) alloca (3 + len1);
  memcpy (path, path1, len1);
  cp = path + len1;
  if (!IS_DIR_SEPARATOR (cp[-1]))
    *cp++ = DIR_SEPARATOR;
  *cp++ = '.';
  *cp = '\0';

  /* Exclude directories that the linker is known to search.  */
  if (linker
      && IS_DIR_SEPARATOR (path[0])
      && ((cp - path == 6
	   && filename_ncmp (path + 1, "lib", 3) == 0)
	  || (cp - path == 10
	      && filename_ncmp (path + 1, "usr", 3) == 0
	      && IS_DIR_SEPARATOR (path[4])
	      && filename_ncmp (path + 5, "lib", 3) == 0)))
    return 0;

  return (stat (path, &st) >= 0 && S_ISDIR (st.st_mode));
}

/* Set up the various global variables to indicate that we're processing
   the input file named FILENAME.  */

void
set_input (const char *filename)
{
  const char *p;

  gcc_input_filename = filename;
  input_filename_length = strlen (gcc_input_filename);
  input_basename = lbasename (gcc_input_filename);

  /* Find a suffix starting with the last period,
     and set basename_length to exclude that suffix.  */
  basename_length = strlen (input_basename);
  suffixed_basename_length = basename_length;
  p = input_basename + basename_length;
  while (p != input_basename && *p != '.')
    --p;
  if (*p == '.' && p != input_basename)
    {
      basename_length = p - input_basename;
      input_suffix = p + 1;
    }
  else
    input_suffix = "";

  /* If a spec for 'g', 'u', or 'U' is seen with -save-temps then
     we will need to do a stat on the gcc_input_filename.  The
     INPUT_STAT_SET signals that the stat is needed.  */
  input_stat_set = 0;
}

/* On fatal signals, delete all the temporary files.  */

static void
fatal_signal (int signum)
{
  signal (signum, SIG_DFL);
  delete_failure_queue ();
  delete_temp_files ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

/* Compare the contents of the two files named CMPFILE[0] and
   CMPFILE[1].  Return zero if they're identical, nonzero
   otherwise.  */

static int
compare_files (char *cmpfile[])
{
  int ret = 0;
  FILE *temp[2] = { NULL, NULL };
  int i;

#if HAVE_MMAP_FILE
  {
    size_t length[2];
    void *map[2] = { NULL, NULL };

    for (i = 0; i < 2; i++)
      {
	struct stat st;

	if (stat (cmpfile[i], &st) < 0 || !S_ISREG (st.st_mode))
	  {
	    error ("%s: could not determine length of compare-debug file %s",
		   gcc_input_filename, cmpfile[i]);
	    ret = 1;
	    break;
	  }

	length[i] = st.st_size;
      }

    if (!ret && length[0] != length[1])
      {
	error ("%s: -fcompare-debug failure (length)", gcc_input_filename);
	ret = 1;
      }

    if (!ret)
      for (i = 0; i < 2; i++)
	{
	  int fd = open (cmpfile[i], O_RDONLY);
	  if (fd < 0)
	    {
	      error ("%s: could not open compare-debug file %s",
		     gcc_input_filename, cmpfile[i]);
	      ret = 1;
	      break;
	    }

	  map[i] = mmap (NULL, length[i], PROT_READ, MAP_PRIVATE, fd, 0);
	  close (fd);

	  if (map[i] == (void *) MAP_FAILED)
	    {
	      ret = -1;
	      break;
	    }
	}

    if (!ret)
      {
	if (memcmp (map[0], map[1], length[0]) != 0)
	  {
	    error ("%s: -fcompare-debug failure", gcc_input_filename);
	    ret = 1;
	  }
      }

    for (i = 0; i < 2; i++)
      if (map[i])
	munmap ((caddr_t) map[i], length[i]);

    if (ret >= 0)
      return ret;

    ret = 0;
  }
#endif

  for (i = 0; i < 2; i++)
    {
      temp[i] = fopen (cmpfile[i], "r");
      if (!temp[i])
	{
	  error ("%s: could not open compare-debug file %s",
		 gcc_input_filename, cmpfile[i]);
	  ret = 1;
	  break;
	}
    }

  if (!ret && temp[0] && temp[1])
    for (;;)
      {
	int c0, c1;
	c0 = fgetc (temp[0]);
	c1 = fgetc (temp[1]);

	if (c0 != c1)
	  {
	    error ("%s: -fcompare-debug failure",
		   gcc_input_filename);
	    ret = 1;
	    break;
	  }

	if (c0 == EOF)
	  break;
      }

  for (i = 1; i >= 0; i--)
    {
      if (temp[i])
	fclose (temp[i]);
    }

  return ret;
}

extern int main (int, char **);

int
main (int argc, char **argv)
{
  size_t i;
  int value;
  int linker_was_run = 0;
  int lang_n_infiles = 0;
  int num_linker_inputs = 0;
  char *explicit_link_files;
  char *specs_file;
  char *lto_wrapper_file;
  const char *p;
  struct user_specs *uptr;
  char **old_argv = argv;
  struct cl_decoded_option *decoded_options;
  unsigned int decoded_options_count;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  expandargv (&argc, &argv);

  /* Determine if any expansions were made.  */
  if (argv != old_argv)
    at_file_supplied = true;

  /* Register the language-independent parameters.  */
  global_init_params ();
  finish_params ();

  init_options_struct (&global_options, &global_options_set);

  decode_cmdline_options_to_array (argc, CONST_CAST2 (const char **, char **,
						      argv),
				   CL_DRIVER,
				   &decoded_options, &decoded_options_count);

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  diagnostic_initialize (global_dc, 0);

#ifdef GCC_DRIVER_HOST_INITIALIZATION
  /* Perform host dependent initialization when needed.  */
  GCC_DRIVER_HOST_INITIALIZATION;
#endif

  if (atexit (delete_temp_files) != 0)
    fatal_error ("atexit failed");

  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, fatal_signal);
#ifdef SIGHUP
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, fatal_signal);
#endif
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, fatal_signal);
#ifdef SIGPIPE
  if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
    signal (SIGPIPE, fatal_signal);
#endif
#ifdef SIGCHLD
  /* We *MUST* set SIGCHLD to SIG_DFL so that the wait4() call will
     receive the signal.  A different setting is inheritable */
  signal (SIGCHLD, SIG_DFL);
#endif

  /* Parsing and gimplification sometimes need quite large stack.
     Increase stack size limits if possible.  */
  stack_limit_increase (64 * 1024 * 1024);

  /* Allocate the argument vector.  */
  alloc_args ();

  obstack_init (&obstack);

  /* Build multilib_select, et. al from the separate lines that make up each
     multilib selection.  */
  {
    const char *const *q = multilib_raw;
    int need_space;

    obstack_init (&multilib_obstack);
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_select = XOBFINISH (&multilib_obstack, const char *);

    q = multilib_matches_raw;
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_matches = XOBFINISH (&multilib_obstack, const char *);

    q = multilib_exclusions_raw;
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_exclusions = XOBFINISH (&multilib_obstack, const char *);

    q = multilib_reuse_raw;
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_reuse = XOBFINISH (&multilib_obstack, const char *);

    need_space = FALSE;
    for (i = 0; i < ARRAY_SIZE (multilib_defaults_raw); i++)
      {
	if (need_space)
	  obstack_1grow (&multilib_obstack, ' ');
	obstack_grow (&multilib_obstack,
		      multilib_defaults_raw[i],
		      strlen (multilib_defaults_raw[i]));
	need_space = TRUE;
      }

    obstack_1grow (&multilib_obstack, 0);
    multilib_defaults = XOBFINISH (&multilib_obstack, const char *);
  }

#ifdef INIT_ENVIRONMENT
  /* Set up any other necessary machine specific environment variables.  */
  xputenv (INIT_ENVIRONMENT);
#endif

  /* Make a table of what switches there are (switches, n_switches).
     Make a table of specified input files (infiles, n_infiles).
     Decode switches that are handled locally.  */

  process_command (decoded_options_count, decoded_options);

  /* Initialize the vector of specs to just the default.
     This means one element containing 0s, as a terminator.  */

  compilers = XNEWVAR (struct compiler, sizeof default_compilers);
  memcpy (compilers, default_compilers, sizeof default_compilers);
  n_compilers = n_default_compilers;

  /* Read specs from a file if there is one.  */

  machine_suffix = concat (spec_machine, dir_separator_str,
			   spec_version, dir_separator_str, NULL);
  just_machine_suffix = concat (spec_machine, dir_separator_str, NULL);

  specs_file = find_a_file (&startfile_prefixes, "specs", R_OK, true);
  /* Read the specs file unless it is a default one.  */
  if (specs_file != 0 && strcmp (specs_file, "specs"))
    read_specs (specs_file, true, false);
  else
    init_spec ();

  /* We need to check standard_exec_prefix/just_machine_suffix/specs
     for any override of as, ld and libraries.  */
  specs_file = (char *) alloca (strlen (standard_exec_prefix)
		       + strlen (just_machine_suffix) + sizeof ("specs"));

  strcpy (specs_file, standard_exec_prefix);
  strcat (specs_file, just_machine_suffix);
  strcat (specs_file, "specs");
  if (access (specs_file, R_OK) == 0)
    read_specs (specs_file, true, false);

  /* Process any configure-time defaults specified for the command line
     options, via OPTION_DEFAULT_SPECS.  */
  for (i = 0; i < ARRAY_SIZE (option_default_specs); i++)
    do_option_spec (option_default_specs[i].name,
		    option_default_specs[i].spec);

  /* Process DRIVER_SELF_SPECS, adding any new options to the end
     of the command line.  */

  for (i = 0; i < ARRAY_SIZE (driver_self_specs); i++)
    do_self_spec (driver_self_specs[i]);

  /* If not cross-compiling, look for executables in the standard
     places.  */
  if (*cross_compile == '0')
    {
      if (*md_exec_prefix)
	{
	  add_prefix (&exec_prefixes, md_exec_prefix, "GCC",
		      PREFIX_PRIORITY_LAST, 0, 0);
	}
    }

  /* Process sysroot_suffix_spec.  */
  if (*sysroot_suffix_spec != 0
      && !no_sysroot_suffix
      && do_spec_2 (sysroot_suffix_spec) == 0)
    {
      if (argbuf.length () > 1)
        error ("spec failure: more than one arg to SYSROOT_SUFFIX_SPEC");
      else if (argbuf.length () == 1)
        target_sysroot_suffix = xstrdup (argbuf.last ());
    }

#ifdef HAVE_LD_SYSROOT
  /* Pass the --sysroot option to the linker, if it supports that.  If
     there is a sysroot_suffix_spec, it has already been processed by
     this point, so target_system_root really is the system root we
     should be using.  */
  if (target_system_root)
    {
      obstack_grow (&obstack, "%(sysroot_spec) ", strlen ("%(sysroot_spec) "));
      obstack_grow0 (&obstack, link_spec, strlen (link_spec));
      set_spec ("link", XOBFINISH (&obstack, const char *), false);
    }
#endif

  /* Process sysroot_hdrs_suffix_spec.  */
  if (*sysroot_hdrs_suffix_spec != 0
      && !no_sysroot_suffix
      && do_spec_2 (sysroot_hdrs_suffix_spec) == 0)
    {
      if (argbuf.length () > 1)
        error ("spec failure: more than one arg to SYSROOT_HEADERS_SUFFIX_SPEC");
      else if (argbuf.length () == 1)
        target_sysroot_hdrs_suffix = xstrdup (argbuf.last ());
    }

  /* Look for startfiles in the standard places.  */
  if (*startfile_prefix_spec != 0
      && do_spec_2 (startfile_prefix_spec) == 0
      && do_spec_1 (" ", 0, NULL) == 0)
    {
      const char *arg;
      int ndx;
      FOR_EACH_VEC_ELT (argbuf, ndx, arg)
	add_sysrooted_prefix (&startfile_prefixes, arg, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
    }
  /* We should eventually get rid of all these and stick to
     startfile_prefix_spec exclusively.  */
  else if (*cross_compile == '0' || target_system_root)
    {
      if (*md_startfile_prefix)
	add_sysrooted_prefix (&startfile_prefixes, md_startfile_prefix,
			      "GCC", PREFIX_PRIORITY_LAST, 0, 1);

      if (*md_startfile_prefix_1)
	add_sysrooted_prefix (&startfile_prefixes, md_startfile_prefix_1,
			      "GCC", PREFIX_PRIORITY_LAST, 0, 1);

      /* If standard_startfile_prefix is relative, base it on
	 standard_exec_prefix.  This lets us move the installed tree
	 as a unit.  If GCC_EXEC_PREFIX is defined, base
	 standard_startfile_prefix on that as well.

         If the prefix is relative, only search it for native compilers;
         otherwise we will search a directory containing host libraries.  */
      if (IS_ABSOLUTE_PATH (standard_startfile_prefix))
	add_sysrooted_prefix (&startfile_prefixes,
			      standard_startfile_prefix, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
      else if (*cross_compile == '0')
	{
	  add_prefix (&startfile_prefixes,
		      concat (gcc_exec_prefix
			      ? gcc_exec_prefix : standard_exec_prefix,
			      machine_suffix,
			      standard_startfile_prefix, NULL),
		      NULL, PREFIX_PRIORITY_LAST, 0, 1);
	}

      /* Sysrooted prefixes are relocated because target_system_root is
	 also relocated by gcc_exec_prefix.  */
      if (*standard_startfile_prefix_1)
 	add_sysrooted_prefix (&startfile_prefixes,
			      standard_startfile_prefix_1, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
      if (*standard_startfile_prefix_2)
	add_sysrooted_prefix (&startfile_prefixes,
			      standard_startfile_prefix_2, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
    }

  /* Process any user specified specs in the order given on the command
     line.  */
  for (uptr = user_specs_head; uptr; uptr = uptr->next)
    {
      char *filename = find_a_file (&startfile_prefixes, uptr->filename,
				    R_OK, true);
      read_specs (filename ? filename : uptr->filename, false, true);
    }

  /* Process any user self specs.  */
  {
    struct spec_list *sl;
    for (sl = specs; sl; sl = sl->next)
      if (sl->name_len == sizeof "self_spec" - 1
	  && !strcmp (sl->name, "self_spec"))
	do_self_spec (*sl->ptr_spec);
  }

  if (compare_debug)
    {
      enum save_temps save;

      if (!compare_debug_second)
	{
	  n_switches_debug_check[1] = n_switches;
	  n_switches_alloc_debug_check[1] = n_switches_alloc;
	  switches_debug_check[1] = XDUPVEC (struct switchstr, switches,
					     n_switches_alloc);

	  do_self_spec ("%:compare-debug-self-opt()");
	  n_switches_debug_check[0] = n_switches;
	  n_switches_alloc_debug_check[0] = n_switches_alloc;
	  switches_debug_check[0] = switches;

	  n_switches = n_switches_debug_check[1];
	  n_switches_alloc = n_switches_alloc_debug_check[1];
	  switches = switches_debug_check[1];
	}

      /* Avoid crash when computing %j in this early.  */
      save = save_temps_flag;
      save_temps_flag = SAVE_TEMPS_NONE;

      compare_debug = -compare_debug;
      do_self_spec ("%:compare-debug-self-opt()");

      save_temps_flag = save;

      if (!compare_debug_second)
	{
	  n_switches_debug_check[1] = n_switches;
	  n_switches_alloc_debug_check[1] = n_switches_alloc;
	  switches_debug_check[1] = switches;
	  compare_debug = -compare_debug;
	  n_switches = n_switches_debug_check[0];
	  n_switches_alloc = n_switches_debug_check[0];
	  switches = switches_debug_check[0];
	}
    }


  /* If we have a GCC_EXEC_PREFIX envvar, modify it for cpp's sake.  */
  if (gcc_exec_prefix)
    gcc_exec_prefix = concat (gcc_exec_prefix, spec_machine, dir_separator_str,
			      spec_version, dir_separator_str, NULL);

  /* Now we have the specs.
     Set the `valid' bits for switches that match anything in any spec.  */

  validate_all_switches ();

  /* Now that we have the switches and the specs, set
     the subdirectory based on the options.  */
  set_multilib_dir ();

  /* Set up to remember the pathname of gcc and any options
     needed for collect.  We use argv[0] instead of progname because
     we need the complete pathname.  */
  obstack_init (&collect_obstack);
  obstack_grow (&collect_obstack, "COLLECT_GCC=", sizeof ("COLLECT_GCC=") - 1);
  obstack_grow (&collect_obstack, argv[0], strlen (argv[0]) + 1);
  xputenv (XOBFINISH (&collect_obstack, char *));

  /* Set up to remember the pathname of the lto wrapper. */

  if (have_c)
    lto_wrapper_file = NULL;
  else
    lto_wrapper_file = find_a_file (&exec_prefixes, "lto-wrapper",
				    X_OK, false);
  if (lto_wrapper_file)
    {
      lto_wrapper_file = convert_white_space (lto_wrapper_file);
      lto_wrapper_spec = lto_wrapper_file;
      obstack_init (&collect_obstack);
      obstack_grow (&collect_obstack, "COLLECT_LTO_WRAPPER=",
		    sizeof ("COLLECT_LTO_WRAPPER=") - 1);
      obstack_grow (&collect_obstack, lto_wrapper_spec,
		    strlen (lto_wrapper_spec) + 1);
      xputenv (XOBFINISH (&collect_obstack, char *));
    }

  /* Reject switches that no pass was interested in.  */

  for (i = 0; (int) i < n_switches; i++)
    if (! switches[i].validated)
      error ("unrecognized command line option %<-%s%>", switches[i].part1);

  /* Obey some of the options.  */

  if (print_search_dirs)
    {
      printf (_("install: %s%s\n"),
	      gcc_exec_prefix ? gcc_exec_prefix : standard_exec_prefix,
	      gcc_exec_prefix ? "" : machine_suffix);
      printf (_("programs: %s\n"),
	      build_search_list (&exec_prefixes, "", false, false));
      printf (_("libraries: %s\n"),
	      build_search_list (&startfile_prefixes, "", false, true));
      return (0);
    }

  if (print_file_name)
    {
      printf ("%s\n", find_file (print_file_name));
      return (0);
    }

  if (print_prog_name)
    {
      char *newname = find_a_file (&exec_prefixes, print_prog_name, X_OK, 0);
      printf ("%s\n", (newname ? newname : print_prog_name));
      return (0);
    }

  if (print_multi_lib)
    {
      print_multilib_info ();
      return (0);
    }

  if (print_multi_directory)
    {
      if (multilib_dir == NULL)
	printf (".\n");
      else
	printf ("%s\n", multilib_dir);
      return (0);
    }

  if (print_multiarch)
    {
      if (multiarch_dir == NULL)
	printf ("\n");
      else
	printf ("%s\n", multiarch_dir);
      return (0);
    }

  if (print_sysroot)
    {
      if (target_system_root)
	{
          if (target_sysroot_suffix)
	    printf ("%s%s\n", target_system_root, target_sysroot_suffix);
          else
	    printf ("%s\n", target_system_root);
	}
      return (0);
    }

  if (print_multi_os_directory)
    {
      if (multilib_os_dir == NULL)
	printf (".\n");
      else
	printf ("%s\n", multilib_os_dir);
      return (0);
    }

  if (print_sysroot_headers_suffix)
    {
      if (*sysroot_hdrs_suffix_spec)
	{
	  printf("%s\n", (target_sysroot_hdrs_suffix
			  ? target_sysroot_hdrs_suffix
			  : ""));
	  return (0);
	}
      else
	/* The error status indicates that only one set of fixed
	   headers should be built.  */
	fatal_error ("not configured with sysroot headers suffix");
    }

  if (print_help_list)
    {
      display_help ();

      if (! verbose_flag)
	{
	  printf (_("\nFor bug reporting instructions, please see:\n"));
	  printf ("%s.\n", bug_report_url);

	  return (0);
	}

      /* We do not exit here.  Instead we have created a fake input file
	 called 'help-dummy' which needs to be compiled, and we pass this
	 on the various sub-processes, along with the --help switch.
	 Ensure their output appears after ours.  */
      fputc ('\n', stdout);
      fflush (stdout);
    }

  if (print_version)
    {
      printf (_("%s %s%s\n"), progname, pkgversion_string,
	      version_string);
      printf ("Copyright %s 2013 Free Software Foundation, Inc.\n",
	      _("(C)"));
      fputs (_("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"),
	     stdout);
      if (! verbose_flag)
	return 0;

      /* We do not exit here. We use the same mechanism of --help to print
	 the version of the sub-processes. */
      fputc ('\n', stdout);
      fflush (stdout);
    }

  if (verbose_flag)
    {
      int n;
      const char *thrmod;

      fnotice (stderr, "Target: %s\n", spec_machine);
      fnotice (stderr, "Configured with: %s\n", configuration_arguments);

#ifdef THREAD_MODEL_SPEC
      /* We could have defined THREAD_MODEL_SPEC to "%*" by default,
	 but there's no point in doing all this processing just to get
	 thread_model back.  */
      obstack_init (&obstack);
      do_spec_1 (THREAD_MODEL_SPEC, 0, thread_model);
      obstack_1grow (&obstack, '\0');
      thrmod = XOBFINISH (&obstack, const char *);
#else
      thrmod = thread_model;
#endif

      fnotice (stderr, "Thread model: %s\n", thrmod);

      /* compiler_version is truncated at the first space when initialized
	 from version string, so truncate version_string at the first space
	 before comparing.  */
      for (n = 0; version_string[n]; n++)
	if (version_string[n] == ' ')
	  break;

      if (! strncmp (version_string, compiler_version, n)
	  && compiler_version[n] == 0)
	fnotice (stderr, "gcc version %s %s\n", version_string,
		 pkgversion_string);
      else
	fnotice (stderr, "gcc driver version %s %sexecuting gcc version %s\n",
		 version_string, pkgversion_string, compiler_version);

      if (n_infiles == 0)
	return (0);
    }

  if (n_infiles == added_libraries)
    fatal_error ("no input files");

  if (seen_error ())
    goto out;

  /* Make a place to record the compiler output file names
     that correspond to the input files.  */

  i = n_infiles;
  i += lang_specific_extra_outfiles;
  outfiles = XCNEWVEC (const char *, i);

  /* Record which files were specified explicitly as link input.  */

  explicit_link_files = XCNEWVEC (char, n_infiles);

  combine_inputs = have_o || flag_wpa;

  for (i = 0; (int) i < n_infiles; i++)
    {
      const char *name = infiles[i].name;
      struct compiler *compiler = lookup_compiler (name,
						   strlen (name),
						   infiles[i].language);

      if (compiler && !(compiler->combinable))
	combine_inputs = false;

      if (lang_n_infiles > 0 && compiler != input_file_compiler
	  && infiles[i].language && infiles[i].language[0] != '*')
	infiles[i].incompiler = compiler;
      else if (compiler)
	{
	  lang_n_infiles++;
	  input_file_compiler = compiler;
	  infiles[i].incompiler = compiler;
	}
      else
	{
	  /* Since there is no compiler for this input file, assume it is a
	     linker file.  */
	  explicit_link_files[i] = 1;
	  infiles[i].incompiler = NULL;
	}
      infiles[i].compiled = false;
      infiles[i].preprocessed = false;
    }

  if (!combine_inputs && have_c && have_o && lang_n_infiles > 1)
    fatal_error ("cannot specify -o with -c, -S or -E with multiple files");

  for (i = 0; (int) i < n_infiles; i++)
    {
      int this_file_error = 0;

      /* Tell do_spec what to substitute for %i.  */

      input_file_number = i;
      set_input (infiles[i].name);

      if (infiles[i].compiled)
	continue;

      /* Use the same thing in %o, unless cp->spec says otherwise.  */

      outfiles[i] = gcc_input_filename;

      /* Figure out which compiler from the file's suffix.  */

      input_file_compiler
	= lookup_compiler (infiles[i].name, input_filename_length,
			   infiles[i].language);

      if (input_file_compiler)
	{
	  /* Ok, we found an applicable compiler.  Run its spec.  */

	  if (input_file_compiler->spec[0] == '#')
	    {
	      error ("%s: %s compiler not installed on this system",
		     gcc_input_filename, &input_file_compiler->spec[1]);
	      this_file_error = 1;
	    }
	  else
	    {
	      if (compare_debug)
		{
		  free (debug_check_temp_file[0]);
		  debug_check_temp_file[0] = NULL;

		  free (debug_check_temp_file[1]);
		  debug_check_temp_file[1] = NULL;
		}

	      value = do_spec (input_file_compiler->spec);
	      infiles[i].compiled = true;
	      if (value < 0)
		this_file_error = 1;
	      else if (compare_debug && debug_check_temp_file[0])
		{
		  if (verbose_flag)
		    inform (0, "recompiling with -fcompare-debug");

		  compare_debug = -compare_debug;
		  n_switches = n_switches_debug_check[1];
		  n_switches_alloc = n_switches_alloc_debug_check[1];
		  switches = switches_debug_check[1];

		  value = do_spec (input_file_compiler->spec);

		  compare_debug = -compare_debug;
		  n_switches = n_switches_debug_check[0];
		  n_switches_alloc = n_switches_alloc_debug_check[0];
		  switches = switches_debug_check[0];

		  if (value < 0)
		    {
		      error ("during -fcompare-debug recompilation");
		      this_file_error = 1;
		    }

		  gcc_assert (debug_check_temp_file[1]
			      && filename_cmp (debug_check_temp_file[0],
					       debug_check_temp_file[1]));

		  if (verbose_flag)
		    inform (0, "comparing final insns dumps");

		  if (compare_files (debug_check_temp_file))
		    this_file_error = 1;
		}

	      if (compare_debug)
		{
		  free (debug_check_temp_file[0]);
		  debug_check_temp_file[0] = NULL;

		  free (debug_check_temp_file[1]);
		  debug_check_temp_file[1] = NULL;
		}
	    }
	}

      /* If this file's name does not contain a recognized suffix,
	 record it as explicit linker input.  */

      else
	explicit_link_files[i] = 1;

      /* Clear the delete-on-failure queue, deleting the files in it
	 if this compilation failed.  */

      if (this_file_error)
	{
	  delete_failure_queue ();
	  errorcount++;
	}
      /* If this compilation succeeded, don't delete those files later.  */
      clear_failure_queue ();
    }

  /* Reset the input file name to the first compile/object file name, for use
     with %b in LINK_SPEC. We use the first input file that we can find
     a compiler to compile it instead of using infiles.language since for
     languages other than C we use aliases that we then lookup later.  */
  if (n_infiles > 0)
    {
      int i;

      for (i = 0; i < n_infiles ; i++)
	if (infiles[i].incompiler
	    || (infiles[i].language && infiles[i].language[0] != '*'))
	  {
	    set_input (infiles[i].name);
	    break;
	  }
    }

  if (!seen_error ())
    {
      /* Make sure INPUT_FILE_NUMBER points to first available open
	 slot.  */
      input_file_number = n_infiles;
      if (lang_specific_pre_link ())
	errorcount++;
    }

  /* Determine if there are any linker input files.  */
  num_linker_inputs = 0;
  for (i = 0; (int) i < n_infiles; i++)
    if (explicit_link_files[i] || outfiles[i] != NULL)
      num_linker_inputs++;

  /* Run ld to link all the compiler output files.  */

  if (num_linker_inputs > 0 && !seen_error () && print_subprocess_help < 2)
    {
      int tmp = execution_count;

      if (! have_c)
	{
#if HAVE_LTO_PLUGIN > 0
#if HAVE_LTO_PLUGIN == 2
	  const char *fno_use_linker_plugin = "fno-use-linker-plugin";
#else
	  const char *fuse_linker_plugin = "fuse-linker-plugin";
#endif
#endif

	  /* We'll use ld if we can't find collect2.  */
	  if (! strcmp (linker_name_spec, "collect2"))
	    {
	      char *s = find_a_file (&exec_prefixes, "collect2", X_OK, false);
	      if (s == NULL)
		linker_name_spec = "ld";
	    }

#if HAVE_LTO_PLUGIN > 0
#if HAVE_LTO_PLUGIN == 2
	  if (!switch_matches (fno_use_linker_plugin,
			       fno_use_linker_plugin
			       + strlen (fno_use_linker_plugin), 0))
#else
	  if (switch_matches (fuse_linker_plugin,
			      fuse_linker_plugin
			      + strlen (fuse_linker_plugin), 0))
#endif
	    {
	      char *temp_spec = find_a_file (&exec_prefixes,
					     LTOPLUGINSONAME, R_OK,
					     false);
	      if (!temp_spec)
		fatal_error ("-fuse-linker-plugin, but %s not found",
			     LTOPLUGINSONAME);
	      linker_plugin_file_spec = convert_white_space (temp_spec);
	    }
#endif
	  lto_gcc_spec = argv[0];
	}

      /* Rebuild the COMPILER_PATH and LIBRARY_PATH environment variables
	 for collect.  */
      putenv_from_prefixes (&exec_prefixes, "COMPILER_PATH", false);
      putenv_from_prefixes (&startfile_prefixes, LIBRARY_PATH_ENV, true);

      if (print_subprocess_help == 1)
	{
	  printf (_("\nLinker options\n==============\n\n"));
	  printf (_("Use \"-Wl,OPTION\" to pass \"OPTION\""
		    " to the linker.\n\n"));
	  fflush (stdout);
	}
      value = do_spec (link_command_spec);
      if (value < 0)
	errorcount = 1;
      linker_was_run = (tmp != execution_count);
    }

  /* If options said don't run linker,
     complain about input files to be given to the linker.  */

  if (! linker_was_run && !seen_error ())
    for (i = 0; (int) i < n_infiles; i++)
      if (explicit_link_files[i]
	  && !(infiles[i].language && infiles[i].language[0] == '*'))
	warning (0, "%s: linker input file unused because linking not done",
		 outfiles[i]);

  /* Delete some or all of the temporary files we made.  */

  if (seen_error ())
    delete_failure_queue ();
  delete_temp_files ();

  if (print_help_list)
    {
      printf (("\nFor bug reporting instructions, please see:\n"));
      printf ("%s\n", bug_report_url);
    }

 out:
  return (signal_count != 0 ? 2
	  : seen_error () ? (pass_exit_codes ? greatest_status : 1)
	  : 0);
}

/* Find the proper compilation spec for the file name NAME,
   whose length is LENGTH.  LANGUAGE is the specified language,
   or 0 if this file is to be passed to the linker.  */

static struct compiler *
lookup_compiler (const char *name, size_t length, const char *language)
{
  struct compiler *cp;

  /* If this was specified by the user to be a linker input, indicate that.  */
  if (language != 0 && language[0] == '*')
    return 0;

  /* Otherwise, look for the language, if one is spec'd.  */
  if (language != 0)
    {
      for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
	if (cp->suffix[0] == '@' && !strcmp (cp->suffix + 1, language))
	  return cp;

      error ("language %s not recognized", language);
      return 0;
    }

  /* Look for a suffix.  */
  for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
    {
      if (/* The suffix `-' matches only the file name `-'.  */
	  (!strcmp (cp->suffix, "-") && !strcmp (name, "-"))
	  || (strlen (cp->suffix) < length
	      /* See if the suffix matches the end of NAME.  */
	      && !strcmp (cp->suffix,
			  name + length - strlen (cp->suffix))
	 ))
	break;
    }

#if defined (OS2) ||defined (HAVE_DOS_BASED_FILE_SYSTEM)
  /* Look again, but case-insensitively this time.  */
  if (cp < compilers)
    for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
      {
	if (/* The suffix `-' matches only the file name `-'.  */
	    (!strcmp (cp->suffix, "-") && !strcmp (name, "-"))
	    || (strlen (cp->suffix) < length
		/* See if the suffix matches the end of NAME.  */
		&& ((!strcmp (cp->suffix,
			     name + length - strlen (cp->suffix))
		     || !strpbrk (cp->suffix, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
		    && !strcasecmp (cp->suffix,
				    name + length - strlen (cp->suffix)))
	   ))
	  break;
      }
#endif

  if (cp >= compilers)
    {
      if (cp->spec[0] != '@')
	/* A non-alias entry: return it.  */
	return cp;

      /* An alias entry maps a suffix to a language.
	 Search for the language; pass 0 for NAME and LENGTH
	 to avoid infinite recursion if language not found.  */
      return lookup_compiler (NULL, 0, cp->spec + 1);
    }
  return 0;
}

static char *
save_string (const char *s, int len)
{
  char *result = XNEWVEC (char, len + 1);

  memcpy (result, s, len);
  result[len] = 0;
  return result;
}

void
pfatal_with_name (const char *name)
{
  perror_with_name (name);
  delete_temp_files ();
  exit (1);
}

static void
perror_with_name (const char *name)
{
  error ("%s: %m", name);
}

static inline void
validate_switches_from_spec (const char *spec, bool user)
{
  const char *p = spec;
  char c;
  while ((c = *p++))
    if (c == '%' && (*p == '{' || *p == '<' || (*p == 'W' && *++p == '{')))
      /* We have a switch spec.  */
      p = validate_switches (p + 1, user);
}

static void
validate_all_switches (void)
{
  struct compiler *comp;
  struct spec_list *spec;

  for (comp = compilers; comp->spec; comp++)
    validate_switches_from_spec (comp->spec, false);

  /* Look through the linked list of specs read from the specs file.  */
  for (spec = specs; spec; spec = spec->next)
    validate_switches_from_spec (*spec->ptr_spec, spec->user_p);

  validate_switches_from_spec (link_command_spec, false);
}

/* Look at the switch-name that comes after START
   and mark as valid all supplied switches that match it.  */

static const char *
validate_switches (const char *start, bool user_spec)
{
  const char *p = start;
  const char *atom;
  size_t len;
  int i;
  bool suffix = false;
  bool starred = false;

#define SKIP_WHITE() do { while (*p == ' ' || *p == '\t') p++; } while (0)

next_member:
  SKIP_WHITE ();

  if (*p == '!')
    p++;

  SKIP_WHITE ();
  if (*p == '.' || *p == ',')
    suffix = true, p++;

  atom = p;
  while (ISIDNUM (*p) || *p == '-' || *p == '+' || *p == '='
	 || *p == ',' || *p == '.' || *p == '@')
    p++;
  len = p - atom;

  if (*p == '*')
    starred = true, p++;

  SKIP_WHITE ();

  if (!suffix)
    {
      /* Mark all matching switches as valid.  */
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, atom, len)
	    && (starred || switches[i].part1[len] == '\0')
	    && (switches[i].known || user_spec))
	      switches[i].validated = true;
    }

  if (*p) p++;
  if (*p && (p[-1] == '|' || p[-1] == '&'))
    goto next_member;

  if (*p && p[-1] == ':')
    {
      while (*p && *p != ';' && *p != '}')
	{
	  if (*p == '%')
	    {
	      p++;
	      if (*p == '{' || *p == '<')
		p = validate_switches (p+1, user_spec);
	      else if (p[0] == 'W' && p[1] == '{')
		p = validate_switches (p+2, user_spec);
	    }
	  else
	    p++;
	}

      if (*p) p++;
      if (*p && p[-1] == ';')
	goto next_member;
    }

  return p;
#undef SKIP_WHITE
}

struct mdswitchstr
{
  const char *str;
  int len;
};

static struct mdswitchstr *mdswitches;
static int n_mdswitches;

/* Check whether a particular argument was used.  The first time we
   canonicalize the switches to keep only the ones we care about.  */

static int
used_arg (const char *p, int len)
{
  struct mswitchstr
  {
    const char *str;
    const char *replace;
    int len;
    int rep_len;
  };

  static struct mswitchstr *mswitches;
  static int n_mswitches;
  int i, j;

  if (!mswitches)
    {
      struct mswitchstr *matches;
      const char *q;
      int cnt = 0;

      /* Break multilib_matches into the component strings of string
         and replacement string.  */
      for (q = multilib_matches; *q != '\0'; q++)
	if (*q == ';')
	  cnt++;

      matches
	= (struct mswitchstr *) alloca ((sizeof (struct mswitchstr)) * cnt);
      i = 0;
      q = multilib_matches;
      while (*q != '\0')
	{
	  matches[i].str = q;
	  while (*q != ' ')
	    {
	      if (*q == '\0')
		{
		invalid_matches:
		  fatal_error ("multilib spec %qs is invalid",
			       multilib_matches);
		}
	      q++;
	    }
	  matches[i].len = q - matches[i].str;

	  matches[i].replace = ++q;
	  while (*q != ';' && *q != '\0')
	    {
	      if (*q == ' ')
		goto invalid_matches;
	      q++;
	    }
	  matches[i].rep_len = q - matches[i].replace;
	  i++;
	  if (*q == ';')
	    q++;
	}

      /* Now build a list of the replacement string for switches that we care
	 about.  Make sure we allocate at least one entry.  This prevents
	 xmalloc from calling fatal, and prevents us from re-executing this
	 block of code.  */
      mswitches
	= XNEWVEC (struct mswitchstr, n_mdswitches + (n_switches ? n_switches : 1));
      for (i = 0; i < n_switches; i++)
	if ((switches[i].live_cond & SWITCH_IGNORE) == 0)
	  {
	    int xlen = strlen (switches[i].part1);
	    for (j = 0; j < cnt; j++)
	      if (xlen == matches[j].len
		  && ! strncmp (switches[i].part1, matches[j].str, xlen))
		{
		  mswitches[n_mswitches].str = matches[j].replace;
		  mswitches[n_mswitches].len = matches[j].rep_len;
		  mswitches[n_mswitches].replace = (char *) 0;
		  mswitches[n_mswitches].rep_len = 0;
		  n_mswitches++;
		  break;
		}
	  }

      /* Add MULTILIB_DEFAULTS switches too, as long as they were not present
	 on the command line nor any options mutually incompatible with
	 them.  */
      for (i = 0; i < n_mdswitches; i++)
	{
	  const char *r;

	  for (q = multilib_options; *q != '\0'; q++)
	    {
	      while (*q == ' ')
		q++;

	      r = q;
	      while (strncmp (q, mdswitches[i].str, mdswitches[i].len) != 0
		     || strchr (" /", q[mdswitches[i].len]) == NULL)
		{
		  while (*q != ' ' && *q != '/' && *q != '\0')
		    q++;
		  if (*q != '/')
		    break;
		  q++;
		}

	      if (*q != ' ' && *q != '\0')
		{
		  while (*r != ' ' && *r != '\0')
		    {
		      q = r;
		      while (*q != ' ' && *q != '/' && *q != '\0')
			q++;

		      if (used_arg (r, q - r))
			break;

		      if (*q != '/')
			{
			  mswitches[n_mswitches].str = mdswitches[i].str;
			  mswitches[n_mswitches].len = mdswitches[i].len;
			  mswitches[n_mswitches].replace = (char *) 0;
			  mswitches[n_mswitches].rep_len = 0;
			  n_mswitches++;
			  break;
			}

		      r = q + 1;
		    }
		  break;
		}
	    }
	}
    }

  for (i = 0; i < n_mswitches; i++)
    if (len == mswitches[i].len && ! strncmp (p, mswitches[i].str, len))
      return 1;

  return 0;
}

static int
default_arg (const char *p, int len)
{
  int i;

  for (i = 0; i < n_mdswitches; i++)
    if (len == mdswitches[i].len && ! strncmp (p, mdswitches[i].str, len))
      return 1;

  return 0;
}

/* Work out the subdirectory to use based on the options. The format of
   multilib_select is a list of elements. Each element is a subdirectory
   name followed by a list of options followed by a semicolon. The format
   of multilib_exclusions is the same, but without the preceding
   directory. First gcc will check the exclusions, if none of the options
   beginning with an exclamation point are present, and all of the other
   options are present, then we will ignore this completely. Passing
   that, gcc will consider each multilib_select in turn using the same
   rules for matching the options. If a match is found, that subdirectory
   will be used.
   A subdirectory name is optionally followed by a colon and the corresponding
   multiarch name.  */

static void
set_multilib_dir (void)
{
  const char *p;
  unsigned int this_path_len;
  const char *this_path, *this_arg;
  const char *start, *end;
  int not_arg;
  int ok, ndfltok, first;

  n_mdswitches = 0;
  start = multilib_defaults;
  while (*start == ' ' || *start == '\t')
    start++;
  while (*start != '\0')
    {
      n_mdswitches++;
      while (*start != ' ' && *start != '\t' && *start != '\0')
	start++;
      while (*start == ' ' || *start == '\t')
        start++;
    }

  if (n_mdswitches)
    {
      int i = 0;

      mdswitches = XNEWVEC (struct mdswitchstr, n_mdswitches);
      for (start = multilib_defaults; *start != '\0'; start = end + 1)
	{
	  while (*start == ' ' || *start == '\t')
	    start++;

	  if (*start == '\0')
	    break;

	  for (end = start + 1;
	       *end != ' ' && *end != '\t' && *end != '\0'; end++)
	    ;

	  obstack_grow (&multilib_obstack, start, end - start);
	  obstack_1grow (&multilib_obstack, 0);
	  mdswitches[i].str = XOBFINISH (&multilib_obstack, const char *);
	  mdswitches[i++].len = end - start;

	  if (*end == '\0')
	    break;
	}
    }

  p = multilib_exclusions;
  while (*p != '\0')
    {
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Check the arguments.  */
      ok = 1;
      while (*p != ';')
	{
	  if (*p == '\0')
	    {
	    invalid_exclusions:
	      fatal_error ("multilib exclusions %qs is invalid",
			   multilib_exclusions);
	    }

	  if (! ok)
	    {
	      ++p;
	      continue;
	    }

	  this_arg = p;
	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		goto invalid_exclusions;
	      ++p;
	    }

	  if (*this_arg != '!')
	    not_arg = 0;
	  else
	    {
	      not_arg = 1;
	      ++this_arg;
	    }

	  ok = used_arg (this_arg, p - this_arg);
	  if (not_arg)
	    ok = ! ok;

	  if (*p == ' ')
	    ++p;
	}

      if (ok)
	return;

      ++p;
    }

  first = 1;
  p = multilib_select;

  /* Append multilib reuse rules if any.  With those rules, we can reuse
     one multilib for certain different options sets.  */
  if (strlen (multilib_reuse) > 0)
    p = concat (p, multilib_reuse, NULL);

  while (*p != '\0')
    {
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Get the initial path.  */
      this_path = p;
      while (*p != ' ')
	{
	  if (*p == '\0')
	    {
	    invalid_select:
	      fatal_error ("multilib select %qs %qs is invalid",
			   multilib_select, multilib_reuse);
	    }
	  ++p;
	}
      this_path_len = p - this_path;

      /* Check the arguments.  */
      ok = 1;
      ndfltok = 1;
      ++p;
      while (*p != ';')
	{
	  if (*p == '\0')
	    goto invalid_select;

	  if (! ok)
	    {
	      ++p;
	      continue;
	    }

	  this_arg = p;
	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		goto invalid_select;
	      ++p;
	    }

	  if (*this_arg != '!')
	    not_arg = 0;
	  else
	    {
	      not_arg = 1;
	      ++this_arg;
	    }

	  /* If this is a default argument, we can just ignore it.
	     This is true even if this_arg begins with '!'.  Beginning
	     with '!' does not mean that this argument is necessarily
	     inappropriate for this library: it merely means that
	     there is a more specific library which uses this
	     argument.  If this argument is a default, we need not
	     consider that more specific library.  */
	  ok = used_arg (this_arg, p - this_arg);
	  if (not_arg)
	    ok = ! ok;

	  if (! ok)
	    ndfltok = 0;

	  if (default_arg (this_arg, p - this_arg))
	    ok = 1;

	  if (*p == ' ')
	    ++p;
	}

      if (ok && first)
	{
	  if (this_path_len != 1
	      || this_path[0] != '.')
	    {
	      char *new_multilib_dir = XNEWVEC (char, this_path_len + 1);
	      char *q;

	      strncpy (new_multilib_dir, this_path, this_path_len);
	      new_multilib_dir[this_path_len] = '\0';
	      q = strchr (new_multilib_dir, ':');
	      if (q != NULL)
		*q = '\0';
	      multilib_dir = new_multilib_dir;
	    }
	  first = 0;
	}

      if (ndfltok)
	{
	  const char *q = this_path, *end = this_path + this_path_len;

	  while (q < end && *q != ':')
	    q++;
	  if (q < end)
	    {
	      const char *q2 = q + 1, *ml_end = end;
	      char *new_multilib_os_dir;

	      while (q2 < end && *q2 != ':')
		q2++;
	      if (*q2 == ':')
		ml_end = q2;
	      new_multilib_os_dir = XNEWVEC (char, ml_end - q);
	      memcpy (new_multilib_os_dir, q + 1, ml_end - q - 1);
	      new_multilib_os_dir[ml_end - q - 1] = '\0';
	      multilib_os_dir = *new_multilib_os_dir ? new_multilib_os_dir : ".";

	      if (q2 < end && *q2 == ':')
		{
		  char *new_multiarch_dir = XNEWVEC (char, end - q2);
		  memcpy (new_multiarch_dir, q2 + 1, end - q2 - 1);
		  new_multiarch_dir[end - q2 - 1] = '\0';
		  multiarch_dir = new_multiarch_dir;
		}
	      break;
	    }
	}

      ++p;
    }

  if (multilib_dir == NULL && multilib_os_dir != NULL
      && strcmp (multilib_os_dir, ".") == 0)
    {
      free (CONST_CAST (char *, multilib_os_dir));
      multilib_os_dir = NULL;
    }
  else if (multilib_dir != NULL && multilib_os_dir == NULL)
    multilib_os_dir = multilib_dir;
}

/* Print out the multiple library subdirectory selection
   information.  This prints out a series of lines.  Each line looks
   like SUBDIRECTORY;@OPTION@OPTION, with as many options as is
   required.  Only the desired options are printed out, the negative
   matches.  The options are print without a leading dash.  There are
   no spaces to make it easy to use the information in the shell.
   Each subdirectory is printed only once.  This assumes the ordering
   generated by the genmultilib script. Also, we leave out ones that match
   the exclusions.  */

static void
print_multilib_info (void)
{
  const char *p = multilib_select;
  const char *last_path = 0, *this_path;
  int skip;
  unsigned int last_path_len = 0;

  while (*p != '\0')
    {
      skip = 0;
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Get the initial path.  */
      this_path = p;
      while (*p != ' ')
	{
	  if (*p == '\0')
	    {
	    invalid_select:
	      fatal_error ("multilib select %qs is invalid", multilib_select);
	    }

	  ++p;
	}

      /* When --disable-multilib was used but target defines
	 MULTILIB_OSDIRNAMES, entries starting with .: (and not starting
         with .:: for multiarch configurations) are there just to find
         multilib_os_dir, so skip them from output.  */
      if (this_path[0] == '.' && this_path[1] == ':' && this_path[2] != ':')
	skip = 1;

      /* Check for matches with the multilib_exclusions. We don't bother
         with the '!' in either list. If any of the exclusion rules match
         all of its options with the select rule, we skip it.  */
      {
	const char *e = multilib_exclusions;
	const char *this_arg;

	while (*e != '\0')
	  {
	    int m = 1;
	    /* Ignore newlines.  */
	    if (*e == '\n')
	      {
		++e;
		continue;
	      }

	    /* Check the arguments.  */
	    while (*e != ';')
	      {
		const char *q;
		int mp = 0;

		if (*e == '\0')
		  {
		  invalid_exclusion:
		    fatal_error ("multilib exclusion %qs is invalid",
				 multilib_exclusions);
		  }

		if (! m)
		  {
		    ++e;
		    continue;
		  }

		this_arg = e;

		while (*e != ' ' && *e != ';')
		  {
		    if (*e == '\0')
		      goto invalid_exclusion;
		    ++e;
		  }

		q = p + 1;
		while (*q != ';')
		  {
		    const char *arg;
		    int len = e - this_arg;

		    if (*q == '\0')
		      goto invalid_select;

		    arg = q;

		    while (*q != ' ' && *q != ';')
		      {
			if (*q == '\0')
			  goto invalid_select;
			++q;
		      }

		    if (! strncmp (arg, this_arg,
				   (len < q - arg) ? q - arg : len)
			|| default_arg (this_arg, e - this_arg))
		      {
			mp = 1;
			break;
		      }

		    if (*q == ' ')
		      ++q;
		  }

		if (! mp)
		  m = 0;

		if (*e == ' ')
		  ++e;
	      }

	    if (m)
	      {
		skip = 1;
		break;
	      }

	    if (*e != '\0')
	      ++e;
	  }
      }

      if (! skip)
	{
	  /* If this is a duplicate, skip it.  */
	  skip = (last_path != 0
		  && (unsigned int) (p - this_path) == last_path_len
		  && ! filename_ncmp (last_path, this_path, last_path_len));

	  last_path = this_path;
	  last_path_len = p - this_path;
	}

      /* If this directory requires any default arguments, we can skip
	 it.  We will already have printed a directory identical to
	 this one which does not require that default argument.  */
      if (! skip)
	{
	  const char *q;

	  q = p + 1;
	  while (*q != ';')
	    {
	      const char *arg;

	      if (*q == '\0')
		goto invalid_select;

	      if (*q == '!')
		arg = NULL;
	      else
		arg = q;

	      while (*q != ' ' && *q != ';')
		{
		  if (*q == '\0')
		    goto invalid_select;
		  ++q;
		}

	      if (arg != NULL
		  && default_arg (arg, q - arg))
		{
		  skip = 1;
		  break;
		}

	      if (*q == ' ')
		++q;
	    }
	}

      if (! skip)
	{
	  const char *p1;

	  for (p1 = last_path; p1 < p && *p1 != ':'; p1++)
	    putchar (*p1);
	  putchar (';');
	}

      ++p;
      while (*p != ';')
	{
	  int use_arg;

	  if (*p == '\0')
	    goto invalid_select;

	  if (skip)
	    {
	      ++p;
	      continue;
	    }

	  use_arg = *p != '!';

	  if (use_arg)
	    putchar ('@');

	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		goto invalid_select;
	      if (use_arg)
		putchar (*p);
	      ++p;
	    }

	  if (*p == ' ')
	    ++p;
	}

      if (! skip)
	{
	  /* If there are extra options, print them now.  */
	  if (multilib_extra && *multilib_extra)
	    {
	      int print_at = TRUE;
	      const char *q;

	      for (q = multilib_extra; *q != '\0'; q++)
		{
		  if (*q == ' ')
		    print_at = TRUE;
		  else
		    {
		      if (print_at)
			putchar ('@');
		      putchar (*q);
		      print_at = FALSE;
		    }
		}
	    }

	  putchar ('\n');
	}

      ++p;
    }
}

/* getenv built-in spec function.

   Returns the value of the environment variable given by its first
   argument, concatenated with the second argument.  If the
   environment variable is not defined, a fatal error is issued.  */

static const char *
getenv_spec_function (int argc, const char **argv)
{
  char *value;
  char *result;
  char *ptr;
  size_t len;

  if (argc != 2)
    return NULL;

  value = getenv (argv[0]);
  if (!value)
    fatal_error ("environment variable %qs not defined", argv[0]);

  /* We have to escape every character of the environment variable so
     they are not interpreted as active spec characters.  A
     particularly painful case is when we are reading a variable
     holding a windows path complete with \ separators.  */
  len = strlen (value) * 2 + strlen (argv[1]) + 1;
  result = XNEWVAR (char, len);
  for (ptr = result; *value; ptr += 2)
    {
      ptr[0] = '\\';
      ptr[1] = *value++;
    }

  strcpy (ptr, argv[1]);

  return result;
}

/* if-exists built-in spec function.

   Checks to see if the file specified by the absolute pathname in
   ARGS exists.  Returns that pathname if found.

   The usual use for this function is to check for a library file
   (whose name has been expanded with %s).  */

static const char *
if_exists_spec_function (int argc, const char **argv)
{
  /* Must have only one argument.  */
  if (argc == 1 && IS_ABSOLUTE_PATH (argv[0]) && ! access (argv[0], R_OK))
    return argv[0];

  return NULL;
}

/* if-exists-else built-in spec function.

   This is like if-exists, but takes an additional argument which
   is returned if the first argument does not exist.  */

static const char *
if_exists_else_spec_function (int argc, const char **argv)
{
  /* Must have exactly two arguments.  */
  if (argc != 2)
    return NULL;

  if (IS_ABSOLUTE_PATH (argv[0]) && ! access (argv[0], R_OK))
    return argv[0];

  return argv[1];
}

/* replace-outfile built-in spec function.

   This looks for the first argument in the outfiles array's name and
   replaces it with the second argument.  */

static const char *
replace_outfile_spec_function (int argc, const char **argv)
{
  int i;
  /* Must have exactly two arguments.  */
  if (argc != 2)
    abort ();

  for (i = 0; i < n_infiles; i++)
    {
      if (outfiles[i] && !filename_cmp (outfiles[i], argv[0]))
	outfiles[i] = xstrdup (argv[1]);
    }
  return NULL;
}

/* remove-outfile built-in spec function.
 *
 *    This looks for the first argument in the outfiles array's name and
 *       removes it.  */

static const char *
remove_outfile_spec_function (int argc, const char **argv)
{
  int i;
  /* Must have exactly one argument.  */
  if (argc != 1)
    abort ();

  for (i = 0; i < n_infiles; i++)
    {
      if (outfiles[i] && !filename_cmp (outfiles[i], argv[0]))
        outfiles[i] = NULL;
    }
  return NULL;
}

/* Given two version numbers, compares the two numbers.
   A version number must match the regular expression
   ([1-9][0-9]*|0)(\.([1-9][0-9]*|0))*
*/
static int
compare_version_strings (const char *v1, const char *v2)
{
  int rresult;
  regex_t r;

  if (regcomp (&r, "^([1-9][0-9]*|0)(\\.([1-9][0-9]*|0))*$",
	       REG_EXTENDED | REG_NOSUB) != 0)
    abort ();
  rresult = regexec (&r, v1, 0, NULL, 0);
  if (rresult == REG_NOMATCH)
    fatal_error ("invalid version number %qs", v1);
  else if (rresult != 0)
    abort ();
  rresult = regexec (&r, v2, 0, NULL, 0);
  if (rresult == REG_NOMATCH)
    fatal_error ("invalid version number %qs", v2);
  else if (rresult != 0)
    abort ();

  return strverscmp (v1, v2);
}


/* version_compare built-in spec function.

   This takes an argument of the following form:

   <comparison-op> <arg1> [<arg2>] <switch> <result>

   and produces "result" if the comparison evaluates to true,
   and nothing if it doesn't.

   The supported <comparison-op> values are:

   >=  true if switch is a later (or same) version than arg1
   !>  opposite of >=
   <   true if switch is an earlier version than arg1
   !<  opposite of <
   ><  true if switch is arg1 or later, and earlier than arg2
   <>  true if switch is earlier than arg1 or is arg2 or later

   If the switch is not present, the condition is false unless
   the first character of the <comparison-op> is '!'.

   For example,
   %:version-compare(>= 10.3 mmacosx-version-min= -lmx)
   adds -lmx if -mmacosx-version-min=10.3.9 was passed.  */

static const char *
version_compare_spec_function (int argc, const char **argv)
{
  int comp1, comp2;
  size_t switch_len;
  const char *switch_value = NULL;
  int nargs = 1, i;
  bool result;

  if (argc < 3)
    fatal_error ("too few arguments to %%:version-compare");
  if (argv[0][0] == '\0')
    abort ();
  if ((argv[0][1] == '<' || argv[0][1] == '>') && argv[0][0] != '!')
    nargs = 2;
  if (argc != nargs + 3)
    fatal_error ("too many arguments to %%:version-compare");

  switch_len = strlen (argv[nargs + 1]);
  for (i = 0; i < n_switches; i++)
    if (!strncmp (switches[i].part1, argv[nargs + 1], switch_len)
	&& check_live_switch (i, switch_len))
      switch_value = switches[i].part1 + switch_len;

  if (switch_value == NULL)
    comp1 = comp2 = -1;
  else
    {
      comp1 = compare_version_strings (switch_value, argv[1]);
      if (nargs == 2)
	comp2 = compare_version_strings (switch_value, argv[2]);
      else
	comp2 = -1;  /* This value unused.  */
    }

  switch (argv[0][0] << 8 | argv[0][1])
    {
    case '>' << 8 | '=':
      result = comp1 >= 0;
      break;
    case '!' << 8 | '<':
      result = comp1 >= 0 || switch_value == NULL;
      break;
    case '<' << 8:
      result = comp1 < 0;
      break;
    case '!' << 8 | '>':
      result = comp1 < 0 || switch_value == NULL;
      break;
    case '>' << 8 | '<':
      result = comp1 >= 0 && comp2 < 0;
      break;
    case '<' << 8 | '>':
      result = comp1 < 0 || comp2 >= 0;
      break;

    default:
      fatal_error ("unknown operator %qs in %%:version-compare", argv[0]);
    }
  if (! result)
    return NULL;

  return argv[nargs + 2];
}

/* %:include builtin spec function.  This differs from %include in that it
   can be nested inside a spec, and thus be conditionalized.  It takes
   one argument, the filename, and looks for it in the startfile path.
   The result is always NULL, i.e. an empty expansion.  */

static const char *
include_spec_function (int argc, const char **argv)
{
  char *file;

  if (argc != 1)
    abort ();

  file = find_a_file (&startfile_prefixes, argv[0], R_OK, true);
  read_specs (file ? file : argv[0], false, false);

  return NULL;
}

/* %:find-file spec function.  This function replaces its argument by
    the file found through find_file, that is the -print-file-name gcc
    program option. */
static const char *
find_file_spec_function (int argc, const char **argv)
{
  const char *file;

  if (argc != 1)
    abort ();

  file = find_file (argv[0]);
  return file;
}


/* %:find-plugindir spec function.  This function replaces its argument
    by the -iplugindir=<dir> option.  `dir' is found through find_file, that
    is the -print-file-name gcc program option. */
static const char *
find_plugindir_spec_function (int argc, const char **argv ATTRIBUTE_UNUSED)
{
  const char *option;

  if (argc != 0)
    abort ();

  option = concat ("-iplugindir=", find_file ("plugin"), NULL);
  return option;
}


/* %:print-asm-header spec function.  Print a banner to say that the
   following output is from the assembler.  */

static const char *
print_asm_header_spec_function (int arg ATTRIBUTE_UNUSED,
				const char **argv ATTRIBUTE_UNUSED)
{
  printf (_("Assembler options\n=================\n\n"));
  printf (_("Use \"-Wa,OPTION\" to pass \"OPTION\" to the assembler.\n\n"));
  fflush (stdout);
  return NULL;
}

/* Get a random number for -frandom-seed */

static unsigned HOST_WIDE_INT
get_random_number (void)
{
  unsigned HOST_WIDE_INT ret = 0;
  int fd; 

  fd = open ("/dev/urandom", O_RDONLY); 
  if (fd >= 0)
    {
      read (fd, &ret, sizeof (HOST_WIDE_INT));
      close (fd);
      if (ret)
        return ret;
    }

  /* Get some more or less random data.  */
#ifdef HAVE_GETTIMEOFDAY
  {
    struct timeval tv;

    gettimeofday (&tv, NULL);
    ret = tv.tv_sec * 1000 + tv.tv_usec / 1000;
  }
#else
  {
    time_t now = time (NULL);

    if (now != (time_t)-1)
      ret = (unsigned) now;
  }
#endif

  return ret ^ getpid();
}

/* %:compare-debug-dump-opt spec function.  Save the last argument,
   expected to be the last -fdump-final-insns option, or generate a
   temporary.  */

static const char *
compare_debug_dump_opt_spec_function (int arg,
				      const char **argv ATTRIBUTE_UNUSED)
{
  char *ret;
  char *name;
  int which;
  static char random_seed[HOST_BITS_PER_WIDE_INT / 4 + 3];

  if (arg != 0)
    fatal_error ("too many arguments to %%:compare-debug-dump-opt");

  do_spec_2 ("%{fdump-final-insns=*:%*}");
  do_spec_1 (" ", 0, NULL);

  if (argbuf.length () > 0
      && strcmp (argv[argbuf.length () - 1], "."))
    {
      if (!compare_debug)
	return NULL;

      name = xstrdup (argv[argbuf.length () - 1]);
      ret = NULL;
    }
  else
    {
      const char *ext = NULL;

      if (argbuf.length () > 0)
	{
	  do_spec_2 ("%{o*:%*}%{!o:%{!S:%b%O}%{S:%b.s}}");
	  ext = ".gkd";
	}
      else if (!compare_debug)
	return NULL;
      else
	do_spec_2 ("%g.gkd");

      do_spec_1 (" ", 0, NULL);

      gcc_assert (argbuf.length () > 0);

      name = concat (argbuf.last (), ext, NULL);

      ret = concat ("-fdump-final-insns=", name, NULL);
    }

  which = compare_debug < 0;
  debug_check_temp_file[which] = name;

  if (!which)
    {
      unsigned HOST_WIDE_INT value = get_random_number ();

      sprintf (random_seed, HOST_WIDE_INT_PRINT_HEX, value);
    }

  if (*random_seed)
    {
      char *tmp = ret;
      ret = concat ("%{!frandom-seed=*:-frandom-seed=", random_seed, "} ",
		    ret, NULL);
      free (tmp);
    }

  if (which)
    *random_seed = 0;

  return ret;
}

static const char *debug_auxbase_opt;

/* %:compare-debug-self-opt spec function.  Expands to the options
    that are to be passed in the second compilation of
    compare-debug.  */

static const char *
compare_debug_self_opt_spec_function (int arg,
				      const char **argv ATTRIBUTE_UNUSED)
{
  if (arg != 0)
    fatal_error ("too many arguments to %%:compare-debug-self-opt");

  if (compare_debug >= 0)
    return NULL;

  do_spec_2 ("%{c|S:%{o*:%*}}");
  do_spec_1 (" ", 0, NULL);

  if (argbuf.length () > 0)
    debug_auxbase_opt = concat ("-auxbase-strip ",
				argbuf.last (),
				NULL);
  else
    debug_auxbase_opt = NULL;

  return concat ("\
%<o %<MD %<MMD %<MF* %<MG %<MP %<MQ* %<MT* \
%<fdump-final-insns=* -w -S -o %j \
%{!fcompare-debug-second:-fcompare-debug-second} \
", compare_debug_opt, NULL);
}

/* %:compare-debug-auxbase-opt spec function.  Expands to the auxbase
    options that are to be passed in the second compilation of
    compare-debug.  It expects, as an argument, the basename of the
    current input file name, with the .gk suffix appended to it.  */

static const char *
compare_debug_auxbase_opt_spec_function (int arg,
					 const char **argv)
{
  char *name;
  int len;

  if (arg == 0)
    fatal_error ("too few arguments to %%:compare-debug-auxbase-opt");

  if (arg != 1)
    fatal_error ("too many arguments to %%:compare-debug-auxbase-opt");

  if (compare_debug >= 0)
    return NULL;

  len = strlen (argv[0]);
  if (len < 3 || strcmp (argv[0] + len - 3, ".gk") != 0)
    fatal_error ("argument to %%:compare-debug-auxbase-opt "
		 "does not end in .gk");

  if (debug_auxbase_opt)
    return debug_auxbase_opt;

#define OPT "-auxbase "

  len -= 3;
  name = (char*) xmalloc (sizeof (OPT) + len);
  memcpy (name, OPT, sizeof (OPT) - 1);
  memcpy (name + sizeof (OPT) - 1, argv[0], len);
  name[sizeof (OPT) - 1 + len] = '\0';

#undef OPT

  return name;
}

/* %:pass-through-libs spec function.  Finds all -l options and input
   file names in the lib spec passed to it, and makes a list of them
   prepended with the plugin option to cause them to be passed through
   to the final link after all the new object files have been added.  */

const char *
pass_through_libs_spec_func (int argc, const char **argv)
{
  char *prepended = xstrdup (" ");
  int n;
  /* Shlemiel the painter's algorithm.  Innately horrible, but at least
     we know that there will never be more than a handful of strings to
     concat, and it's only once per run, so it's not worth optimising.  */
  for (n = 0; n < argc; n++)
    {
      char *old = prepended;
      /* Anything that isn't an option is a full path to an output
         file; pass it through if it ends in '.a'.  Among options,
	 pass only -l.  */
      if (argv[n][0] == '-' && argv[n][1] == 'l')
	{
	  const char *lopt = argv[n] + 2;
	  /* Handle both joined and non-joined -l options.  If for any
	     reason there's a trailing -l with no joined or following
	     arg just discard it.  */
	  if (!*lopt && ++n >= argc)
	    break;
	  else if (!*lopt)
	    lopt = argv[n];
	  prepended = concat (prepended, "-plugin-opt=-pass-through=-l",
		lopt, " ", NULL);
	}
      else if (!strcmp (".a", argv[n] + strlen (argv[n]) - 2))
	{
	  prepended = concat (prepended, "-plugin-opt=-pass-through=",
		argv[n], " ", NULL);
	}
      if (prepended != old)
	free (old);
    }
  return prepended;
}

/* %:replace-extension spec function.  Replaces the extension of the
   first argument with the second argument.  */

const char *
replace_extension_spec_func (int argc, const char **argv)
{
  char *name;
  char *p;
  char *result;
  int i;

  if (argc != 2)
    fatal_error ("too few arguments to %%:replace-extension");

  name = xstrdup (argv[0]);

  for (i = strlen(name) - 1; i >= 0; i--)
    if (IS_DIR_SEPARATOR (name[i]))
      break;

  p = strrchr (name + i + 1, '.');
  if (p != NULL)
      *p = '\0';

  result = concat (name, argv[1], NULL);

  free (name);
  return result;
}

/* Insert backslash before spaces in ORIG (usually a file path), to 
   avoid being broken by spec parser.

   This function is needed as do_spec_1 treats white space (' ' and '\t')
   as the end of an argument. But in case of -plugin /usr/gcc install/xxx.so,
   the file name should be treated as a single argument rather than being
   broken into multiple. Solution is to insert '\\' before the space in a 
   file name.
   
   This function converts and only converts all occurrence of ' ' 
   to '\\' + ' ' and '\t' to '\\' + '\t'.  For example:
   "a b"  -> "a\\ b"
   "a  b" -> "a\\ \\ b"
   "a\tb" -> "a\\\tb"
   "a\\ b" -> "a\\\\ b"

   orig: input null-terminating string that was allocated by xalloc. The
   memory it points to might be freed in this function. Behavior undefined
   if ORIG wasn't xalloced or was freed already at entry.

   Return: ORIG if no conversion needed. Otherwise a newly allocated string
   that was converted from ORIG.  */

static char *
convert_white_space (char *orig)
{
  int len, number_of_space = 0;

  for (len = 0; orig[len]; len++)
    if (orig[len] == ' ' || orig[len] == '\t') number_of_space++;

  if (number_of_space)
    {
      char *new_spec = (char *) xmalloc (len + number_of_space + 1);
      int j, k;
      for (j = 0, k = 0; j <= len; j++, k++)
	{
	  if (orig[j] == ' ' || orig[j] == '\t')
	    new_spec[k++] = '\\';
	  new_spec[k] = orig[j];
	}
      free (orig);
      return new_spec;
  }
  else
    return orig;
}
