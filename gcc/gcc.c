/* Compiler driver program that can handle many languages.
   Copyright (C) 1987, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

This paragraph is here to try to keep Sun CC from dying.
The number of chars here seems crucial!!!!  */

/* This program is the user interface to the C compiler and possibly to
other compilers.  It is used because compilation is a complicated procedure
which involves running several programs and passing temporary files between
them, forwarding the users switches to those programs selectively,
and deleting the temporary files at the end.

CC recognizes how to compile each input file by suffixes in the file names.
Once it knows which kind of compilation to perform, the procedure for
compilation is specified by a string called a "spec".  */

#include <sys/types.h>
#include <ctype.h>
#include <signal.h>
#include <sys/stat.h>
#include <errno.h>

#ifndef _WIN32
#include <sys/file.h>   /* May get R_OK, etc. on some systems.  */
#else
#include <process.h>
int __spawnv ();
int __spawnvp ();
#endif

#include "config.h"
#include "obstack.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>

/* Include multi-lib information.  */
#include "multilib.h"

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#endif

#ifndef WIFSIGNALED
#define WIFSIGNALED(S) (((S) & 0xff) != 0 && ((S) & 0xff) != 0x7f)
#endif
#ifndef WTERMSIG
#define WTERMSIG(S) ((S) & 0x7f)
#endif
#ifndef WIFEXITED
#define WIFEXITED(S) (((S) & 0xff) == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(S) (((S) & 0xff00) >> 8)
#endif

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

#ifndef VPROTO
#ifdef __STDC__
#define PVPROTO(ARGS)		ARGS
#define VPROTO(ARGS)		ARGS
#define VA_START(va_list,var)	va_start(va_list,var)
#else
#define PVPROTO(ARGS)		()
#define VPROTO(ARGS)		(va_alist) va_dcl
#define VA_START(va_list,var)	va_start(va_list)
#endif
#endif

/* Define a generic NULL if one hasn't already been defined.  */

#ifndef NULL
#define NULL 0
#endif

/* Define O_RDONLY if the system hasn't defined it for us. */
#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef GENERIC_PTR
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define GENERIC_PTR void *
#else
#define GENERIC_PTR char *
#endif
#endif

#ifndef NULL_PTR
#define NULL_PTR ((GENERIC_PTR)0)
#endif

#ifdef USG
#define vfork fork
#endif /* USG */

/* On MSDOS, write temp files in current dir
   because there's no place else we can expect to use.  */
#ifdef __MSDOS__
#ifndef P_tmpdir
#define P_tmpdir "."
#endif
#endif

/* Test if something is a normal file.  */
#ifndef S_ISREG
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif

/* Test if something is a directory.  */
#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

/* By default there is no special suffix for executables.  */
#ifndef EXECUTABLE_SUFFIX
#define EXECUTABLE_SUFFIX ""
#endif

/* By default, the suffix for object files is ".o". */
#ifdef OBJECT_SUFFIX
#define HAVE_OBJECT_SUFFIX
#else
#define OBJECT_SUFFIX ".o"
#endif

/* By default, colon separates directories in a path.  */
#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ':'
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

static char dir_separator_str[] = {DIR_SEPARATOR, 0};

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();
extern char *getenv ();

#ifndef errno
extern int errno;
#endif

extern int sys_nerr;
#ifndef HAVE_STRERROR
#if defined(bsd4_4)
extern const char *const sys_errlist[];
#else
extern char *sys_errlist[];
#endif
#else
extern char *strerror();
#endif

extern int execv (), execvp ();

/* If a stage of compilation returns an exit status >= 1,
   compilation of that file ceases.  */

#define MIN_FATAL_STATUS 1

/* Flag saying to print the directories gcc will search through looking for
   programs, libraries, etc.  */

static int print_search_dirs;

/* Flag saying to print the full filename of this file
   as found through our usual search mechanism.  */

static char *print_file_name = NULL;

/* As print_file_name, but search for executable file. */

static char *print_prog_name = NULL;

/* Flag saying to print the relative path we'd use to
   find libgcc.a given the current compiler flags.  */

static int print_multi_directory;

/* Flag saying to print the list of subdirectories and
   compiler flags used to select them in a standard form.  */

static int print_multi_lib;

/* Flag indicating whether we should print the command and arguments */

static int verbose_flag;

/* Nonzero means write "temp" files in source directory
   and use the source file's name in them, and don't delete them.  */

static int save_temps_flag;

/* The compiler version.  */

static char *compiler_version;

/* The target version specified with -V */

static char *spec_version = DEFAULT_TARGET_VERSION;

/* The target machine specified with -b.  */

static char *spec_machine = DEFAULT_TARGET_MACHINE;

/* Nonzero if cross-compiling.
   When -b is used, the value comes from the `specs' file.  */

#ifdef CROSS_COMPILE
static int cross_compile = 1;
#else
static int cross_compile = 0;
#endif

/* The number of errors that have occurred; the link phase will not be
   run if this is non-zero.  */
static int error_count = 0;

/* This is the obstack which we use to allocate many strings.  */

static struct obstack obstack;

/* This is the obstack to build an environment variable to pass to
   collect2 that describes all of the relevant switches of what to
   pass the compiler in building the list of pointers to constructors
   and destructors.  */

static struct obstack collect_obstack;

extern char *version_string;

/* Forward declaration for prototypes.  */
struct path_prefix;

static void set_spec		PROTO((char *, char *));
static struct compiler *lookup_compiler PROTO((char *, int, char *));
static char *build_search_list	PROTO((struct path_prefix *, char *, int));
static void putenv_from_prefixes PROTO((struct path_prefix *, char *));
static char *find_a_file	PROTO((struct path_prefix *, char *, int));
static void add_prefix		PROTO((struct path_prefix *, char *, int, int, int *));
static char *skip_whitespace	PROTO((char *));
static void record_temp_file	PROTO((char *, int, int));
static void delete_if_ordinary	PROTO((char *));
static void delete_temp_files	PROTO((void));
static void delete_failure_queue PROTO((void));
static void clear_failure_queue PROTO((void));
static char *choose_temp_base_try PROTO((char *, char *));
static void choose_temp_base	PROTO((void));
static int check_live_switch	PROTO((int, int));
static char *handle_braces	PROTO((char *));
static char *save_string	PROTO((char *, int));
static char *concat		PROTO((char *, char *));
static char *concat3		PROTO((char *, char *, char *));
static char *concat4		PROTO((char *, char *, char *, char *));
static char *concat6		PROTO((char *, char *, char *, char *, char *, \
                                       char *));
static int do_spec		PROTO((char *));
static int do_spec_1		PROTO((char *, int, char *));
static char *find_file		PROTO((char *));
static int is_directory		PROTO((char *, char *, int));
static void validate_switches	PROTO((char *));
static void validate_all_switches PROTO((void));
static void give_switch		PROTO((int, int));
static int used_arg		PROTO((char *, int));
static int default_arg		PROTO((char *, int));
static void set_multilib_dir	PROTO((void));
static void print_multilib_info	PROTO((void));
static void pfatal_with_name	PROTO((char *));
static void perror_with_name	PROTO((char *));
static void perror_exec		PROTO((char *));
#ifdef HAVE_VPRINTF
static void fatal		PVPROTO((char *, ...));
static void error		PVPROTO((char *, ...));
#else
/* We must not provide any prototype here, even if ANSI C.  */
static void fatal		PROTO(());
static void error		PROTO(());
#endif

void fancy_abort ();
char *xmalloc ();
char *xrealloc ();

/* Specs are strings containing lines, each of which (if not blank)
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
	and not including the directory.
 %g     substitute the temporary-file-name-base.  This is a string chosen
	once per compilation.  Different temporary file names are made by
	concatenation of constant strings on the end, as in `%g.s'.
	%g also has the same effect of %d.
 %u	like %g, but make the temporary file name unique.
 %U	returns the last file name generated with %u.
 %d	marks the argument containing or following the %d as a
	temporary file name, so that that file will be deleted if CC exits
	successfully.  Unlike %g, this contributes no text to the argument.
 %w	marks the argument containing or following the %w as the
	"output file" of this compilation.  This puts the argument
	into the sequence of arguments that %o will substitute later.
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
 %O	substitutes the suffix for object files.
 %p	substitutes the standard macro predefinitions for the
	current target machine.  Use this when running cpp.
 %P	like %p, but puts `__' before and after the name of each macro.
	(Except macros that already have __.)
	This is for ANSI C.
 %I	Substitute a -iprefix option made from GCC_EXEC_PREFIX.
 %s     current argument is the name of a library or startup file of some sort.
        Search for that file in a standard list of directories
	and substitute the full name found.
 %eSTR  Print STR as an error message.  STR is terminated by a newline.
        Use this when inconsistent options are detected.
 %x{OPTION}	Accumulate an option for %X.
 %X	Output the accumulated linker options specified by compilations.
 %Y	Output the accumulated assembler options specified by compilations.
 %Z	Output the accumulated preprocessor options specified by compilations.
 %v1	Substitute the major version number of GCC.
	(For version 2.5.n, this is 2.)
 %v2	Substitute the minor version number of GCC.
	(For version 2.5.n, this is 5.)
 %a     process ASM_SPEC as a spec.
        This allows config.h to specify part of the spec for running as.
 %A	process ASM_FINAL_SPEC as a spec.  A capital A is actually
	used here.  This can be used to run a post-processor after the
	assembler has done it's job.
 %D	Dump out a -L option for each directory in startfile_prefixes.
	If multilib_dir is set, extra entries are generated with it affixed.
 %l     process LINK_SPEC as a spec.
 %L     process LIB_SPEC as a spec.
 %G     process LIBGCC_SPEC as a spec.
 %S     process STARTFILE_SPEC as a spec.  A capital S is actually used here.
 %E     process ENDFILE_SPEC as a spec.  A capital E is actually used here.
 %c	process SIGNED_CHAR_SPEC as a spec.
 %C     process CPP_SPEC as a spec.  A capital C is actually used here.
 %1	process CC1_SPEC as a spec.
 %2	process CC1PLUS_SPEC as a spec.
 %|	output "-" if the input for the current command is coming from a pipe.
 %*	substitute the variable part of a matched option.  (See below.)
	Note that each comma in the substituted string is replaced by
	a single space.
 %{S}   substitutes the -S switch, if that switch was given to CC.
	If that switch was not specified, this substitutes nothing.
	Here S is a metasyntactic variable.
 %{S*}  substitutes all the switches specified to CC whose names start
	with -S.  This is used for -o, -D, -I, etc; switches that take
	arguments.  CC considers `-o foo' as being one switch whose
	name starts with `o'.  %{o*} would substitute this text,
	including the space; thus, two arguments would be generated.
 %{S*:X} substitutes X if one or more switches whose names start with -S are
	specified to CC.  Note that the tail part of the -S option
	(i.e. the part matched by the `*') will be substituted for each
	occurrence of %* within X.
 %{S:X} substitutes X, but only if the -S switch was given to CC.
 %{!S:X} substitutes X, but only if the -S switch was NOT given to CC.
 %{|S:X} like %{S:X}, but if no S switch, substitute `-'.
 %{|!S:X} like %{!S:X}, but if there is an S switch, substitute `-'.
 %{.S:X} substitutes X, but only if processing a file with suffix S.
 %{!.S:X} substitutes X, but only if NOT processing a file with suffix S.
 %(Spec) processes a specification defined in a specs file as *Spec:
 %[Spec] as above, but put __ around -D arguments

The conditional text X in a %{S:X} or %{!S:X} construct may contain
other nested % constructs or spaces, or even newlines.  They are
processed as usual, as described above.

The -O, -f, -m, and -W switches are handled specifically in these
constructs.  If another value of -O or the negated form of a -f, -m, or
-W switch is found later in the command line, the earlier switch
value is ignored, except with {S*} where S is just one letter; this
passes all matching options.

The character | is used to indicate that a command should be piped to
the following command, but only if -pipe is specified.

Note that it is built into CC which switches take arguments and which
do not.  You might think it would be useful to generalize this to
allow each compiler's spec to say which switches take arguments.  But
this cannot be done in a consistent fashion.  CC cannot even decide
which input files have been specified without knowing which switches
take arguments, and it must know which input files to compile in order
to tell which compilers to run.

CC also knows implicitly that arguments starting in `-l' are to be
treated as compiler output files, and passed to the linker in their
proper position among the other output files.  */

/* Define the macros used for specs %a, %l, %L, %S, %c, %C, %1.  */

/* config.h can define ASM_SPEC to provide extra args to the assembler
   or extra switch-translations.  */
#ifndef ASM_SPEC
#define ASM_SPEC ""
#endif

/* config.h can define ASM_FINAL_SPEC to run a post processor after
   the assembler has run.  */
#ifndef ASM_FINAL_SPEC
#define ASM_FINAL_SPEC ""
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

/* config.h can define LIBGCC_SPEC to override how and when libgcc.a is
   included.  */
#ifndef LIBGCC_SPEC
#if defined(LINK_LIBGCC_SPECIAL) || defined(LINK_LIBGCC_SPECIAL_1)
/* Have gcc do the search for libgcc.a.  */
#define LIBGCC_SPEC "%{!shared:libgcc.a%s}"
#else
#define LIBGCC_SPEC "%{!shared:-lgcc}"
#endif
#endif

/* config.h can define STARTFILE_SPEC to override the default crt0 files.  */
#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}}"
#endif

/* config.h can define SWITCHES_NEED_SPACES to control passing -o and -L.
   Make the string nonempty to require spaces there.  */
#ifndef SWITCHES_NEED_SPACES
#define SWITCHES_NEED_SPACES ""
#endif

/* config.h can define ENDFILE_SPEC to override the default crtn files.  */
#ifndef ENDFILE_SPEC
#define ENDFILE_SPEC ""
#endif

/* This spec is used for telling cpp whether char is signed or not.  */
#ifndef SIGNED_CHAR_SPEC
/* Use #if rather than ?:
   because MIPS C compiler rejects like ?: in initializers.  */
#if DEFAULT_SIGNED_CHAR
#define SIGNED_CHAR_SPEC "%{funsigned-char:-D__CHAR_UNSIGNED__}"
#else
#define SIGNED_CHAR_SPEC "%{!fsigned-char:-D__CHAR_UNSIGNED__}"
#endif
#endif

/* MULTILIB_SELECT comes from multilib.h.  It gives a
   string interpreted by set_multilib_dir to select a library
   subdirectory based on the compiler options.  */
#ifndef MULTILIB_SELECT
#define MULTILIB_SELECT ". ;"
#endif

static char *cpp_spec = CPP_SPEC;
static char *cpp_predefines = CPP_PREDEFINES;
static char *cc1_spec = CC1_SPEC;
static char *cc1plus_spec = CC1PLUS_SPEC;
static char *signed_char_spec = SIGNED_CHAR_SPEC;
static char *asm_spec = ASM_SPEC;
static char *asm_final_spec = ASM_FINAL_SPEC;
static char *link_spec = LINK_SPEC;
static char *lib_spec = LIB_SPEC;
static char *libgcc_spec = LIBGCC_SPEC;
static char *endfile_spec = ENDFILE_SPEC;
static char *startfile_spec = STARTFILE_SPEC;
static char *switches_need_spaces = SWITCHES_NEED_SPACES;
static char *multilib_select = MULTILIB_SELECT;

/* This defines which switch letters take arguments.  */

#ifndef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)      \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'm' || (CHAR) == 'x' \
   || (CHAR) == 'L' || (CHAR) == 'A')
#endif

/* This defines which multi-letter switches take arguments.  */

#define DEFAULT_WORD_SWITCH_TAKES_ARG(STR)		\
 (!strcmp (STR, "Tdata") || !strcmp (STR, "Ttext")	\
  || !strcmp (STR, "Tbss") || !strcmp (STR, "include")	\
  || !strcmp (STR, "imacros") || !strcmp (STR, "aux-info") \
  || !strcmp (STR, "idirafter") || !strcmp (STR, "iprefix") \
  || !strcmp (STR, "iwithprefix") || !strcmp (STR, "iwithprefixbefore") \
  || !strcmp (STR, "isystem"))

#ifndef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) DEFAULT_WORD_SWITCH_TAKES_ARG (STR)
#endif

/* Record the mapping from file suffixes for compilation specs.  */

struct compiler
{
  char *suffix;			/* Use this compiler for input files
				   whose names end in this suffix.  */

  char *spec[4];		/* To use this compiler, concatenate these
				   specs and pass to do_spec.  */
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

static struct compiler default_compilers[] =
{
  {".c", "@c"},
  {"@c",
   "cpp -lang-c%{ansi:89} %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	%{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG}\
        -undef -D__GNUC__=%v1 -D__GNUC_MINOR__=%v2\
	%{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
	%{!undef:%{!ansi:%p} %P} %{trigraphs} \
        %c %{O*:%{!O0:-D__OPTIMIZE__}} %{traditional} %{ftraditional:-traditional}\
        %{traditional-cpp:-traditional}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
        %i %{!M:%{!MM:%{!E:%{!pipe:%g.i}}}}%{E:%W{o*}}%{M:%W{o*}}%{MM:%W{o*}} |\n",
   "%{!M:%{!MM:%{!E:cc1 %{!pipe:%g.i} %1 \
		   %{!Q:-quiet} -dumpbase %b.c %{d*} %{m*} %{a}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*} %{ansi} \
		   %{traditional} %{v:-version} %{pg:-p} %{p} %{f*}\
		   %{aux-info*}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
              %{!S:as %a %Y\
		      %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
                      %{!pipe:%g.s} %A\n }}}}"},
  {"-",
   "%{E:cpp -lang-c%{ansi:89} %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	%{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG}\
        -undef -D__GNUC__=%v1 -D__GNUC_MINOR__=%v2\
	%{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
	%{!undef:%{!ansi:%p} %P} %{trigraphs}\
        %c %{O*:%{!O0:-D__OPTIMIZE__}} %{traditional} %{ftraditional:-traditional}\
        %{traditional-cpp:-traditional}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
        %i %W{o*}}\
    %{!E:%e-E required when input is from standard input}"},
  {".m", "@objective-c"},
  {"@objective-c",
   "cpp -lang-objc %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	%{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG}\
        -undef -D__OBJC__ -D__GNUC__=%v1 -D__GNUC_MINOR__=%v2\
	 %{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
	%{!undef:%{!ansi:%p} %P} %{trigraphs}\
        %c %{O*:%{!O0:-D__OPTIMIZE__}} %{traditional} %{ftraditional:-traditional}\
        %{traditional-cpp:-traditional}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
        %i %{!M:%{!MM:%{!E:%{!pipe:%g.i}}}}%{E:%W{o*}}%{M:%W{o*}}%{MM:%W{o*}} |\n",
   "%{!M:%{!MM:%{!E:cc1obj %{!pipe:%g.i} %1 \
		   %{!Q:-quiet} -dumpbase %b.m %{d*} %{m*} %{a}\
		   %{g*} %{O*} %{W*} %{w} %{pedantic*} %{ansi} \
		   %{traditional} %{v:-version} %{pg:-p} %{p} %{f*} \
    		   -lang-objc %{gen-decls} \
		   %{aux-info*}\
		   %{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
		   %{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
              %{!S:as %a %Y\
		      %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
                      %{!pipe:%g.s} %A\n }}}}"},
  {".h", "@c-header"},
  {"@c-header",
   "%{!E:%eCompilation of header file requested} \
    cpp %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	 %{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG}\
        -undef -D__GNUC__=%v1 -D__GNUC_MINOR__=%v2\
	 %{ansi:-trigraphs -$ -D__STRICT_ANSI__}\
	%{!undef:%{!ansi:%p} %P} %{trigraphs}\
        %c %{O*:%{!O0:-D__OPTIMIZE__}} %{traditional} %{ftraditional:-traditional}\
        %{traditional-cpp:-traditional}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
        %i %W{o*}"},
  {".i", "@cpp-output"},
  {"@cpp-output",
   "%{!M:%{!MM:%{!E:cc1 %i %1 %{!Q:-quiet} %{d*} %{m*} %{a}\
			%{g*} %{O*} %{W*} %{w} %{pedantic*} %{ansi}\
			%{traditional} %{v:-version} %{pg:-p} %{p} %{f*}\
			%{aux-info*}\
			%{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
			%{S:%W{o*}%{!o*:-o %b.s}}%{!S:-o %{|!pipe:%g.s}} |\n\
		     %{!S:as %a %Y\
			     %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
			     %{!pipe:%g.s} %A\n }}}}"},
  {".s", "@assembler"},
  {"@assembler",
   "%{!M:%{!MM:%{!E:%{!S:as %a %Y\
		            %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
			    %i %A\n }}}}"},
  {".S", "@assembler-with-cpp"},
  {"@assembler-with-cpp",
   "cpp -lang-asm %{nostdinc*} %{C} %{v} %{A*} %{I*} %{P} %I\
	%{C:%{!E:%eGNU C does not support -C without using -E}}\
	%{M} %{MM} %{MD:-MD %b.d} %{MMD:-MMD %b.d} %{MG} %{trigraphs}\
        -undef -$ %{!undef:%p %P} -D__ASSEMBLER__ \
        %c %{O*:%{!O0:-D__OPTIMIZE__}} %{traditional} %{ftraditional:-traditional}\
        %{traditional-cpp:-traditional}\
	%{g*} %{W*} %{w} %{pedantic*} %{H} %{d*} %C %{D*} %{U*} %{i*} %Z\
        %i %{!M:%{!MM:%{!E:%{!pipe:%g.s}}}}%{E:%W{o*}}%{M:%W{o*}}%{MM:%W{o*}} |\n",
   "%{!M:%{!MM:%{!E:%{!S:as %a %Y\
                    %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}\
		    %{!pipe:%g.s} %A\n }}}}"},
#include "specs.h"
  /* Mark end of table */
  {0, 0}
};

/* Number of elements in default_compilers, not counting the terminator.  */

static int n_default_compilers
  = (sizeof default_compilers / sizeof (struct compiler)) - 1;

/* Here is the spec for running the linker, after compiling all files.  */

/* -u* was put back because both BSD and SysV seem to support it.  */
/* %{static:} simply prevents an error message if the target machine
   doesn't handle -static.  */
/* We want %{T*} after %{L*} and %D so that it can be used to specify linker
   scripts which exist in user specified directories, or in standard
   directories.  */
#ifdef LINK_LIBGCC_SPECIAL
/* Don't generate -L options.  */
static char *link_command_spec = "\
%{!fsyntax-only: \
 %{!c:%{!M:%{!MM:%{!E:%{!S:ld %l %X %{o*} %{A} %{d} %{e*} %{m} %{N} %{n} \
			%{r} %{s} %{t} %{u*} %{x} %{z} %{Z}\
			%{!A:%{!nostdlib:%{!nostartfiles:%S}}}\
			%{static:} %{L*} %{T*} %o\
			%{!nostdlib:%{!nodefaultlibs:%G %L %G}}\
			%{!A:%{!nostdlib:%{!nostartfiles:%E}}}\n }}}}}}";
#else
/* Use -L.  */
static char *link_command_spec = "\
%{!fsyntax-only: \
 %{!c:%{!M:%{!MM:%{!E:%{!S:ld %l %X %{o*} %{A} %{d} %{e*} %{m} %{N} %{n} \
			%{r} %{s} %{t} %{u*} %{x} %{z} %{Z}\
			%{!A:%{!nostdlib:%{!nostartfiles:%S}}}\
			%{static:} %{L*} %D %{T*} %o\
			%{!nostdlib:%{!nodefaultlibs:%G %L %G}}\
			%{!A:%{!nostdlib:%{!nostartfiles:%E}}}\n }}}}}}";
#endif

/* A vector of options to give to the linker.
   These options are accumulated by %x,
   and substituted into the linker command with %X.  */
static int n_linker_options;
static char **linker_options;

/* A vector of options to give to the assembler.
   These options are accumulated by -Wa,
   and substituted into the assembler command with %Y.  */
static int n_assembler_options;
static char **assembler_options;

/* A vector of options to give to the preprocessor.
   These options are accumulated by -Wp,
   and substituted into the preprocessor command with %Z.  */
static int n_preprocessor_options;
static char **preprocessor_options;

/* Define how to map long options into short ones.  */

/* This structure describes one mapping.  */
struct option_map
{
  /* The long option's name.  */
  char *name;
  /* The equivalent short option.  */
  char *equivalent;
  /* Argument info.  A string of flag chars; NULL equals no options.
     a => argument required.
     o => argument optional.
     j => join argument to equivalent, making one word.
     * => require other text after NAME as an argument.  */
  char *arg_info;
};

/* This is the table of mappings.  Mappings are tried sequentially
   for each option encountered; the first one that matches, wins.  */

struct option_map option_map[] =
 {
   {"--all-warnings", "-Wall", 0},
   {"--ansi", "-ansi", 0},
   {"--assemble", "-S", 0},
   {"--assert", "-A", "a"},
   {"--comments", "-C", 0},
   {"--compile", "-c", 0},
   {"--debug", "-g", "oj"},
   {"--define-macro", "-D", "a"},
   {"--dependencies", "-M", 0},
   {"--dump", "-d", "a"},
   {"--dumpbase", "-dumpbase", "a"},
   {"--entry", "-e", 0},
   {"--extra-warnings", "-W", 0},
   {"--for-assembler", "-Wa", "a"},
   {"--for-linker", "-Xlinker", "a"},
   {"--force-link", "-u", "a"},
   {"--imacros", "-imacros", "a"},
   {"--include", "-include", "a"},
   {"--include-barrier", "-I-", 0},
   {"--include-directory", "-I", "a"},
   {"--include-directory-after", "-idirafter", "a"},
   {"--include-prefix", "-iprefix", "a"},
   {"--include-with-prefix", "-iwithprefix", "a"},
   {"--include-with-prefix-before", "-iwithprefixbefore", "a"},
   {"--include-with-prefix-after", "-iwithprefix", "a"},
   {"--language", "-x", "a"},
   {"--library-directory", "-L", "a"},
   {"--machine", "-m", "aj"},
   {"--machine-", "-m", "*j"},
   {"--no-line-commands", "-P", 0},
   {"--no-precompiled-includes", "-noprecomp", 0},
   {"--no-standard-includes", "-nostdinc", 0},
   {"--no-standard-libraries", "-nostdlib", 0},
   {"--no-warnings", "-w", 0},
   {"--optimize", "-O", "oj"},
   {"--output", "-o", "a"},
   {"--pedantic", "-pedantic", 0},
   {"--pedantic-errors", "-pedantic-errors", 0},
   {"--pipe", "-pipe", 0},
   {"--prefix", "-B", "a"},
   {"--preprocess", "-E", 0},
   {"--print-search-dirs", "-print-search-dirs", 0},
   {"--print-file-name", "-print-file-name=", "aj"},
   {"--print-libgcc-file-name", "-print-libgcc-file-name", 0},
   {"--print-missing-file-dependencies", "-MG", 0},
   {"--print-multi-lib", "-print-multi-lib", 0},
   {"--print-multi-directory", "-print-multi-directory", 0},
   {"--print-prog-name", "-print-prog-name=", "aj"},
   {"--profile", "-p", 0},
   {"--profile-blocks", "-a", 0},
   {"--quiet", "-q", 0},
   {"--save-temps", "-save-temps", 0},
   {"--shared", "-shared", 0},
   {"--silent", "-q", 0},
   {"--static", "-static", 0},
   {"--symbolic", "-symbolic", 0},
   {"--target", "-b", "a"},
   {"--trace-includes", "-H", 0},
   {"--traditional", "-traditional", 0},
   {"--traditional-cpp", "-traditional-cpp", 0},
   {"--trigraphs", "-trigraphs", 0},
   {"--undefine-macro", "-U", "a"},
   {"--use-version", "-V", "a"},
   {"--user-dependencies", "-MM", 0},
   {"--verbose", "-v", 0},
   {"--version", "-dumpversion", 0},
   {"--warn-", "-W", "*j"},
   {"--write-dependencies", "-MD", 0},
   {"--write-user-dependencies", "-MMD", 0},
   {"--", "-f", "*j"}
 };

/* Translate the options described by *ARGCP and *ARGVP.
   Make a new vector and store it back in *ARGVP,
   and store its length in *ARGVC.  */

static void
translate_options (argcp, argvp)
     int *argcp;
     char ***argvp;
{
  int i, j, k;
  int argc = *argcp;
  char **argv = *argvp;
  char **newv = (char **) xmalloc ((argc + 2) * 2 * sizeof (char *));
  int newindex = 0;

  i = 0;
  newv[newindex++] = argv[i++];

  while (i < argc)
    {
      /* Translate -- options.  */
      if (argv[i][0] == '-' && argv[i][1] == '-')
	{
	  /* Find a mapping that applies to this option.  */
	  for (j = 0; j < sizeof (option_map) / sizeof (option_map[0]); j++)
	    {
	      int optlen = strlen (option_map[j].name);
	      int arglen = strlen (argv[i]);
	      int complen = arglen > optlen ? optlen : arglen;
	      char *arginfo = option_map[j].arg_info;

	      if (arginfo == 0)
		arginfo = "";

	      if (!strncmp (argv[i], option_map[j].name, complen))
		{
		  char *arg = 0;

		  if (arglen < optlen)
		    {
		      for (k = j + 1;
			   k < sizeof (option_map) / sizeof (option_map[0]);
			   k++)
			if (strlen (option_map[k].name) >= arglen
			    && !strncmp (argv[i], option_map[k].name, arglen))
			  {
			    error ("Ambiguous abbreviation %s", argv[i]);
			    break;
			  }

		      if (k != sizeof (option_map) / sizeof (option_map[0]))
			break;
		    }

		  if (arglen > optlen)
		    {
		      /* If the option has an argument, accept that.  */
		      if (argv[i][optlen] == '=')
			arg = argv[i] + optlen + 1;

		      /* If this mapping requires extra text at end of name,
			 accept that as "argument".  */
		      else if (index (arginfo, '*') != 0)
			arg = argv[i] + optlen;

		      /* Otherwise, extra text at end means mismatch.
			 Try other mappings.  */
		      else
			continue;
		    }

		  else if (index (arginfo, '*') != 0)
		    {
		      error ("Incomplete `%s' option", option_map[j].name);
		      break;
		    }

		  /* Handle arguments.  */
		  if (index (arginfo, 'a') != 0)
		    {
		      if (arg == 0)
			{
			  if (i + 1 == argc)
			    {
			      error ("Missing argument to `%s' option",
				     option_map[j].name);
			      break;
			    }

			  arg = argv[++i];
			}
		    }
		  else if (index (arginfo, '*') != 0)
		    ;
		  else if (index (arginfo, 'o') == 0)
		    {
		      if (arg != 0)
			error ("Extraneous argument to `%s' option",
			       option_map[j].name);
		      arg = 0;
		    }

		  /* Store the translation as one argv elt or as two.  */
		  if (arg != 0 && index (arginfo, 'j') != 0)
		    newv[newindex++] = concat (option_map[j].equivalent, arg);
		  else if (arg != 0)
		    {
		      newv[newindex++] = option_map[j].equivalent;
		      newv[newindex++] = arg;
		    }
		  else
		    newv[newindex++] = option_map[j].equivalent;

		  break;
		}
	    }
	  i++;
	}

      /* Handle old-fashioned options--just copy them through,
	 with their arguments.  */
      else if (argv[i][0] == '-')
	{
	  char *p = argv[i] + 1;
	  int c = *p;
	  int nskip = 1;

	  if (SWITCH_TAKES_ARG (c) > (p[1] != 0))
	    nskip += SWITCH_TAKES_ARG (c) - (p[1] != 0);
	  else if (WORD_SWITCH_TAKES_ARG (p))
	    nskip += WORD_SWITCH_TAKES_ARG (p);
	  else if ((c == 'B' || c == 'b' || c == 'V' || c == 'x')
		   && p[1] == 0)
	    nskip += 1;
	  else if (! strcmp (p, "Xlinker"))
	    nskip += 1;

	  /* Watch out for an option at the end of the command line that
	     is missing arguments, and avoid skipping past the end of the
	     command line.  */
	  if (nskip + i > argc)
	    nskip = argc - i;

	  while (nskip > 0)
	    {
	      newv[newindex++] = argv[i++];
	      nskip--;
	    }
	}
      else
	/* Ordinary operands, or +e options.  */
	newv[newindex++] = argv[i++];
    }

  newv[newindex] = 0;

  *argvp = newv;
  *argcp = newindex;
}

char *
my_strerror(e)
     int e;
{

#ifdef HAVE_STRERROR
  return strerror(e);

#else

  static char buffer[30];
  if (!e)
    return "";

  if (e > 0 && e < sys_nerr)
    return sys_errlist[e];

  sprintf (buffer, "Unknown error %d", e);
  return buffer;
#endif
}

/* Read compilation specs from a file named FILENAME,
   replacing the default ones.

   A suffix which starts with `*' is a definition for
   one of the machine-specific sub-specs.  The "suffix" should be
   *asm, *cc1, *cpp, *link, *startfile, *signed_char, etc.
   The corresponding spec is stored in asm_spec, etc.,
   rather than in the `compilers' vector.

   Anything invalid in the file is a fatal error.  */

static void
read_specs (filename)
     char *filename;
{
  int desc;
  int readlen;
  struct stat statbuf;
  char *buffer;
  register char *p;

  if (verbose_flag)
    fprintf (stderr, "Reading specs from %s\n", filename);

  /* Open and stat the file.  */
  desc = open (filename, O_RDONLY, 0);
  if (desc < 0)
    pfatal_with_name (filename);
  if (stat (filename, &statbuf) < 0)
    pfatal_with_name (filename);

  /* Read contents of file into BUFFER.  */
  buffer = xmalloc ((unsigned) statbuf.st_size + 1);
  readlen = read (desc, buffer, (unsigned) statbuf.st_size);
  if (readlen < 0)
    pfatal_with_name (filename);
  buffer[readlen] = 0;
  close (desc);

  /* Scan BUFFER for specs, putting them in the vector.  */
  p = buffer;
  while (1)
    {
      char *suffix;
      char *spec;
      char *in, *out, *p1, *p2;

      /* Advance P in BUFFER to the next nonblank nocomment line.  */
      p = skip_whitespace (p);
      if (*p == 0)
	break;

      /* Find the colon that should end the suffix.  */
      p1 = p;
      while (*p1 && *p1 != ':' && *p1 != '\n') p1++;
      /* The colon shouldn't be missing.  */
      if (*p1 != ':')
	fatal ("specs file malformed after %d characters", p1 - buffer);
      /* Skip back over trailing whitespace.  */
      p2 = p1;
      while (p2 > buffer && (p2[-1] == ' ' || p2[-1] == '\t')) p2--;
      /* Copy the suffix to a string.  */
      suffix = save_string (p, p2 - p);
      /* Find the next line.  */
      p = skip_whitespace (p1 + 1);
      if (p[1] == 0)
	fatal ("specs file malformed after %d characters", p - buffer);
      p1 = p;
      /* Find next blank line.  */
      while (*p1 && !(*p1 == '\n' && p1[1] == '\n')) p1++;
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
	    {
	      while (*in && *in != '\n') in++;
	    }
	  else
	    *out++ = *in++;
	}
      *out = 0;

      if (suffix[0] == '*')
	{
	  if (! strcmp (suffix, "*link_command"))
	    link_command_spec = spec;
	  else
	    set_spec (suffix + 1, spec);
	}
      else
	{
	  /* Add this pair to the vector.  */
	  compilers
	    = ((struct compiler *)
	       xrealloc (compilers, (n_compilers + 2) * sizeof (struct compiler)));
	  compilers[n_compilers].suffix = suffix;
	  bzero ((char *) compilers[n_compilers].spec,
		 sizeof compilers[n_compilers].spec);
	  compilers[n_compilers].spec[0] = spec;
	  n_compilers++;
	  bzero ((char *) &compilers[n_compilers],
		 sizeof compilers[n_compilers]);
	}

      if (*suffix == 0)
	link_command_spec = spec;
    }

  if (link_command_spec == 0)
    fatal ("spec file has no spec for linking");
}

static char *
skip_whitespace (p)
     char *p;
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
	  while (*p != '\n') p++;
	  p++;
	}
      else
	break;
    }

  return p;
}

/* Structure to keep track of the specs that have been defined so far.  These
   are accessed using %(specname) or %[specname] in a compiler or link spec. */

struct spec_list
{
  char *name;                 /* Name of the spec. */
  char *spec;                 /* The spec itself. */
  struct spec_list *next;     /* Next spec in linked list. */
};

/* List of specs that have been defined so far. */

static struct spec_list *specs = (struct spec_list *) 0;

/* Change the value of spec NAME to SPEC.  If SPEC is empty, then the spec is
   removed; If the spec starts with a + then SPEC is added to the end of the
   current spec. */

static void
set_spec (name, spec)
     char *name;
     char *spec;
{
  struct spec_list *sl;
  char *old_spec;

  /* See if the spec already exists */
  for (sl = specs; sl; sl = sl->next)
    if (strcmp (sl->name, name) == 0)
      break;

  if (!sl)
    {
      /* Not found - make it */
      sl = (struct spec_list *) xmalloc (sizeof (struct spec_list));
      sl->name = save_string (name, strlen (name));
      sl->spec = save_string ("", 0);
      sl->next = specs;
      specs = sl;
    }

  old_spec = sl->spec;
  if (name && spec[0] == '+' && isspace (spec[1]))
    sl->spec = concat (old_spec, spec + 1);
  else
    sl->spec = save_string (spec, strlen (spec));

  if (! strcmp (name, "asm"))
    asm_spec = sl->spec;
  else if (! strcmp (name, "asm_final"))
    asm_final_spec = sl->spec;
  else if (! strcmp (name, "cc1"))
    cc1_spec = sl->spec;
  else if (! strcmp (name, "cc1plus"))
    cc1plus_spec = sl->spec;
  else if (! strcmp (name, "cpp"))
    cpp_spec = sl->spec;
  else if (! strcmp (name, "endfile"))
    endfile_spec = sl->spec;
  else if (! strcmp (name, "lib"))
    lib_spec = sl->spec;
  else if (! strcmp (name, "libgcc"))
    libgcc_spec = sl->spec;
  else if (! strcmp (name, "link"))
    link_spec = sl->spec;
  else if (! strcmp (name, "predefines"))
    cpp_predefines = sl->spec;
  else if (! strcmp (name, "signed_char"))
    signed_char_spec = sl->spec;
  else if (! strcmp (name, "startfile"))
    startfile_spec = sl->spec;
  else if (! strcmp (name, "switches_need_spaces"))
    switches_need_spaces = sl->spec;
  else if (! strcmp (name, "cross_compile"))
    cross_compile = atoi (sl->spec);
  else if (! strcmp (name, "multilib"))
    multilib_select = sl->spec;
  /* Free the old spec */
  if (old_spec)
    free (old_spec);
}

/* Accumulate a command (program name and args), and run it.  */

/* Vector of pointers to arguments in the current line of specifications.  */

static char **argbuf;

/* Number of elements allocated in argbuf.  */

static int argbuf_length;

/* Number of elements in argbuf currently in use (containing args).  */

static int argbuf_index;

/* This is the list of suffixes and codes (%g/%u/%U) and the associated
   temp file.  Used only if MKTEMP_EACH_FILE.  */

static struct temp_name {
  char *suffix;		/* suffix associated with the code.  */
  int length;		/* strlen (suffix).  */
  int unique;		/* Indicates whether %g or %u/%U was used.  */
  char *filename;	/* associated filename.  */
  int filename_length;	/* strlen (filename).  */
  struct temp_name *next;
} *temp_names;

/* Number of commands executed so far.  */

static int execution_count;

/* Number of commands that exited with a signal.  */

static int signal_count;

/* Name with which this program was invoked.  */

static char *programname;

/* Structures to keep track of prefixes to try when looking for files. */

struct prefix_list
{
  char *prefix;               /* String to prepend to the path. */
  struct prefix_list *next;   /* Next in linked list. */
  int require_machine_suffix; /* Don't use without machine_suffix.  */
  /* 2 means try both machine_suffix and just_machine_suffix.  */
  int *used_flag_ptr;	      /* 1 if a file was found with this prefix.  */
};

struct path_prefix
{
  struct prefix_list *plist;  /* List of prefixes to try */
  int max_len;                /* Max length of a prefix in PLIST */
  char *name;                 /* Name of this list (used in config stuff) */
};

/* List of prefixes to try when looking for executables. */

static struct path_prefix exec_prefixes = { 0, 0, "exec" };

/* List of prefixes to try when looking for startup (crt0) files. */

static struct path_prefix startfile_prefixes = { 0, 0, "startfile" };

/* List of prefixes to try when looking for include files.  */

static struct path_prefix include_prefixes = { 0, 0, "include" };

/* Suffix to attach to directories searched for commands.
   This looks like `MACHINE/VERSION/'.  */

static char *machine_suffix = 0;

/* Suffix to attach to directories searched for commands.
   This is just `MACHINE/'.  */

static char *just_machine_suffix = 0;

/* Adjusted value of GCC_EXEC_PREFIX envvar.  */

static char *gcc_exec_prefix;

/* Default prefixes to attach to command names.  */

#ifdef CROSS_COMPILE  /* Don't use these prefixes for a cross compiler.  */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef MD_STARTFILE_PREFIX_1
#endif

#ifndef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX "/usr/local/lib/gcc-lib/"
#endif /* !defined STANDARD_EXEC_PREFIX */

static char *standard_exec_prefix = STANDARD_EXEC_PREFIX;
static char *standard_exec_prefix_1 = "/usr/lib/gcc/";
#ifdef MD_EXEC_PREFIX
static char *md_exec_prefix = MD_EXEC_PREFIX;
#endif

#ifndef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/local/lib/"
#endif /* !defined STANDARD_STARTFILE_PREFIX */

#ifdef MD_STARTFILE_PREFIX
static char *md_startfile_prefix = MD_STARTFILE_PREFIX;
#endif
#ifdef MD_STARTFILE_PREFIX_1
static char *md_startfile_prefix_1 = MD_STARTFILE_PREFIX_1;
#endif
static char *standard_startfile_prefix = STANDARD_STARTFILE_PREFIX;
static char *standard_startfile_prefix_1 = "/lib/";
static char *standard_startfile_prefix_2 = "/usr/lib/";

#ifndef TOOLDIR_BASE_PREFIX
#define TOOLDIR_BASE_PREFIX "/usr/local/"
#endif
static char *tooldir_base_prefix = TOOLDIR_BASE_PREFIX;
static char *tooldir_prefix;

/* Subdirectory to use for locating libraries.  Set by
   set_multilib_dir based on the compilation options.  */

static char *multilib_dir;

/* Clear out the vector of arguments (after a command is executed).  */

static void
clear_args ()
{
  argbuf_index = 0;
}

/* Add one argument to the vector at the end.
   This is done when a space is seen or at the end of the line.
   If DELETE_ALWAYS is nonzero, the arg is a filename
    and the file should be deleted eventually.
   If DELETE_FAILURE is nonzero, the arg is a filename
    and the file should be deleted if this compilation fails.  */

static void
store_arg (arg, delete_always, delete_failure)
     char *arg;
     int delete_always, delete_failure;
{
  if (argbuf_index + 1 == argbuf_length)
    {
      argbuf = (char **) xrealloc (argbuf, (argbuf_length *= 2) * sizeof (char *));
    }

  argbuf[argbuf_index++] = arg;
  argbuf[argbuf_index] = 0;

  if (delete_always || delete_failure)
    record_temp_file (arg, delete_always, delete_failure);
}

/* Record the names of temporary files we tell compilers to write,
   and delete them at the end of the run.  */

/* This is the common prefix we use to make temp file names.
   It is chosen once for each run of this program.
   It is substituted into a spec by %g.
   Thus, all temp file names contain this prefix.
   In practice, all temp file names start with this prefix.

   This prefix comes from the envvar TMPDIR if it is defined;
   otherwise, from the P_tmpdir macro if that is defined;
   otherwise, in /usr/tmp or /tmp.  */

static char *temp_filename;

/* Length of the prefix.  */

static int temp_filename_length;

/* Define the list of temporary files to delete.  */

struct temp_file
{
  char *name;
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

static void
record_temp_file (filename, always_delete, fail_delete)
     char *filename;
     int always_delete;
     int fail_delete;
{
  register char *name;
  name = xmalloc (strlen (filename) + 1);
  strcpy (name, filename);

  if (always_delete)
    {
      register struct temp_file *temp;
      for (temp = always_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already1;
      temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
      temp->next = always_delete_queue;
      temp->name = name;
      always_delete_queue = temp;
    already1:;
    }

  if (fail_delete)
    {
      register struct temp_file *temp;
      for (temp = failure_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already2;
      temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
      temp->next = failure_delete_queue;
      temp->name = name;
      failure_delete_queue = temp;
    already2:;
    }
}

/* Delete all the temporary files whose names we previously recorded.  */

static void
delete_if_ordinary (name)
     char *name;
{
  struct stat st;
#ifdef DEBUG
  int i, c;

  printf ("Delete %s? (y or n) ", name);
  fflush (stdout);
  i = getchar ();
  if (i != '\n')
    while ((c = getchar ()) != '\n' && c != EOF) ;
  if (i == 'y' || i == 'Y')
#endif /* DEBUG */
    if (stat (name, &st) >= 0 && S_ISREG (st.st_mode))
      if (unlink (name) < 0)
	if (verbose_flag)
	  perror_with_name (name);
}

static void
delete_temp_files ()
{
  register struct temp_file *temp;

  for (temp = always_delete_queue; temp; temp = temp->next)
    delete_if_ordinary (temp->name);
  always_delete_queue = 0;
}

/* Delete all the files to be deleted on error.  */

static void
delete_failure_queue ()
{
  register struct temp_file *temp;

  for (temp = failure_delete_queue; temp; temp = temp->next)
    delete_if_ordinary (temp->name);
}

static void
clear_failure_queue ()
{
  failure_delete_queue = 0;
}

/* Compute a string to use as the base of all temporary file names.
   It is substituted for %g.  */

static char *
choose_temp_base_try (try, base)
     char *try;
     char *base;
{
  char *rv;
  if (base)
    rv = base;
  else if (try == (char *)0)
    rv = 0;
  else if (access (try, R_OK | W_OK) != 0)
    rv = 0;
  else
    rv = try;
  return rv;
}

static void
choose_temp_base ()
{
  char *base = 0;
  int len;

  base = choose_temp_base_try (getenv ("TMPDIR"), base);
  base = choose_temp_base_try (getenv ("TMP"), base);
  base = choose_temp_base_try (getenv ("TEMP"), base);

#ifdef P_tmpdir
  base = choose_temp_base_try (P_tmpdir, base);
#endif

  base = choose_temp_base_try (concat4 (dir_separator_str, "usr", 
                                        dir_separator_str, "tmp"), 
                                base);
  base = choose_temp_base_try (concat (dir_separator_str, "tmp"), base);
 
  /* If all else fails, use the current directory! */  
  if (base == (char *)0) base = concat(".", dir_separator_str);

  len = strlen (base);
  temp_filename = xmalloc (len + strlen (concat (dir_separator_str, 
                                                 "ccXXXXXX")) + 1);
  strcpy (temp_filename, base);
  if (len > 0 && temp_filename[len-1] != '/'
      && temp_filename[len-1] != DIR_SEPARATOR)
    temp_filename[len++] = DIR_SEPARATOR;
  strcpy (temp_filename + len, "ccXXXXXX");

  mktemp (temp_filename);
  temp_filename_length = strlen (temp_filename);
  if (temp_filename_length == 0)
    abort ();
}


/* Routine to add variables to the environment.  We do this to pass
   the pathname of the gcc driver, and the directories search to the
   collect2 program, which is being run as ld.  This way, we can be
   sure of executing the right compiler when collect2 wants to build
   constructors and destructors.  Since the environment variables we
   use come from an obstack, we don't have to worry about allocating
   space for them.  */

#ifndef HAVE_PUTENV

void
putenv (str)
     char *str;
{
#ifndef VMS			/* nor about VMS */

  extern char **environ;
  char **old_environ = environ;
  char **envp;
  int num_envs = 0;
  int name_len = 1;
  int str_len = strlen (str);
  char *p = str;
  int ch;

  while ((ch = *p++) != '\0' && ch != '=')
    name_len++;

  if (!ch)
    abort ();

  /* Search for replacing an existing environment variable, and
     count the number of total environment variables.  */
  for (envp = old_environ; *envp; envp++)
    {
      num_envs++;
      if (!strncmp (str, *envp, name_len))
	{
	  *envp = str;
	  return;
	}
    }

  /* Add a new environment variable */
  environ = (char **) xmalloc (sizeof (char *) * (num_envs+2));
  *environ = str;
  bcopy ((char *) old_environ, (char *) (environ + 1),
	 sizeof (char *) * (num_envs+1));

#endif	/* VMS */
}

#endif	/* HAVE_PUTENV */


/* Build a list of search directories from PATHS.
   PREFIX is a string to prepend to the list.
   If CHECK_DIR_P is non-zero we ensure the directory exists.
   This is used mostly by putenv_from_prefixes so we use `collect_obstack'.
   It is also used by the --print-search-dirs flag.  */

static char *
build_search_list (paths, prefix, check_dir_p)
     struct path_prefix *paths;
     char *prefix;
     int check_dir_p;
{
  int suffix_len = (machine_suffix) ? strlen (machine_suffix) : 0;
  int just_suffix_len
    = (just_machine_suffix) ? strlen (just_machine_suffix) : 0;
  int first_time = TRUE;
  struct prefix_list *pprefix;

  obstack_grow (&collect_obstack, prefix, strlen (prefix));

  for (pprefix = paths->plist; pprefix != 0; pprefix = pprefix->next)
    {
      int len = strlen (pprefix->prefix);

      if (machine_suffix
	  && (!check_dir_p
	      || is_directory (pprefix->prefix, machine_suffix, 0)))
	{
	  if (!first_time)
	    obstack_1grow (&collect_obstack, PATH_SEPARATOR);
	    
	  first_time = FALSE;
	  obstack_grow (&collect_obstack, pprefix->prefix, len);
	  obstack_grow (&collect_obstack, machine_suffix, suffix_len);
	}

      if (just_machine_suffix
	  && pprefix->require_machine_suffix == 2
	  && (!check_dir_p
	      || is_directory (pprefix->prefix, just_machine_suffix, 0)))
	{
	  if (!first_time)
	    obstack_1grow (&collect_obstack, PATH_SEPARATOR);
	    
	  first_time = FALSE;
	  obstack_grow (&collect_obstack, pprefix->prefix, len);
	  obstack_grow (&collect_obstack, just_machine_suffix,
			just_suffix_len);
	}

      if (!pprefix->require_machine_suffix)
	{
	  if (!first_time)
	    obstack_1grow (&collect_obstack, PATH_SEPARATOR);

	  first_time = FALSE;
	  obstack_grow (&collect_obstack, pprefix->prefix, len);
	}
    }
  obstack_1grow (&collect_obstack, '\0');
  return obstack_finish (&collect_obstack);
}

/* Rebuild the COMPILER_PATH and LIBRARY_PATH environment variables for collect.  */

static void
putenv_from_prefixes (paths, env_var)
     struct path_prefix *paths;
     char *env_var;
{
  putenv (build_search_list (paths, env_var, 1));
}

/* Search for NAME using the prefix list PREFIXES.  MODE is passed to
   access to check permissions.
   Return 0 if not found, otherwise return its name, allocated with malloc. */

static char *
find_a_file (pprefix, name, mode)
     struct path_prefix *pprefix;
     char *name;
     int mode;
{
  char *temp;
  char *file_suffix = ((mode & X_OK) != 0 ? EXECUTABLE_SUFFIX : "");
  struct prefix_list *pl;
  int len = pprefix->max_len + strlen (name) + strlen (file_suffix) + 1;

  if (machine_suffix)
    len += strlen (machine_suffix);

  temp = xmalloc (len);

  /* Determine the filename to execute (special case for absolute paths).  */

  if (*name == '/' || *name == DIR_SEPARATOR)
    {
      if (access (name, mode))
	{
	  strcpy (temp, name);
	  return temp;
	}
    }
  else
    for (pl = pprefix->plist; pl; pl = pl->next)
      {
	if (machine_suffix)
	  {
	    /* Some systems have a suffix for executable files.
	       So try appending that first.  */
	    if (file_suffix[0] != 0)
	      {
		strcpy (temp, pl->prefix);
		strcat (temp, machine_suffix);
		strcat (temp, name);
		strcat (temp, file_suffix);
		if (access (temp, mode) == 0)
		  {
		    if (pl->used_flag_ptr != 0)
		      *pl->used_flag_ptr = 1;
		    return temp;
		  }
	      }

	    /* Now try just the name.  */
	    strcpy (temp, pl->prefix);
	    strcat (temp, machine_suffix);
	    strcat (temp, name);
	    if (access (temp, mode) == 0)
	      {
		if (pl->used_flag_ptr != 0)
		  *pl->used_flag_ptr = 1;
		return temp;
	      }
	  }

	/* Certain prefixes are tried with just the machine type,
	   not the version.  This is used for finding as, ld, etc.  */
	if (just_machine_suffix && pl->require_machine_suffix == 2)
	  {
	    /* Some systems have a suffix for executable files.
	       So try appending that first.  */
	    if (file_suffix[0] != 0)
	      {
		strcpy (temp, pl->prefix);
		strcat (temp, just_machine_suffix);
		strcat (temp, name);
		strcat (temp, file_suffix);
		if (access (temp, mode) == 0)
		  {
		    if (pl->used_flag_ptr != 0)
		      *pl->used_flag_ptr = 1;
		    return temp;
		  }
	      }

	    strcpy (temp, pl->prefix);
	    strcat (temp, just_machine_suffix);
	    strcat (temp, name);
	    if (access (temp, mode) == 0)
	      {
		if (pl->used_flag_ptr != 0)
		  *pl->used_flag_ptr = 1;
		return temp;
	      }
	  }

	/* Certain prefixes can't be used without the machine suffix
	   when the machine or version is explicitly specified.  */
	if (!pl->require_machine_suffix)
	  {
	    /* Some systems have a suffix for executable files.
	       So try appending that first.  */
	    if (file_suffix[0] != 0)
	      {
		strcpy (temp, pl->prefix);
		strcat (temp, name);
		strcat (temp, file_suffix);
		if (access (temp, mode) == 0)
		  {
		    if (pl->used_flag_ptr != 0)
		      *pl->used_flag_ptr = 1;
		    return temp;
		  }
	      }

	    strcpy (temp, pl->prefix);
	    strcat (temp, name);
	    if (access (temp, mode) == 0)
	      {
		if (pl->used_flag_ptr != 0)
		  *pl->used_flag_ptr = 1;
		return temp;
	      }
	  }
      }

  free (temp);
  return 0;
}

/* Add an entry for PREFIX in PLIST.  If FIRST is set, it goes
   at the start of the list, otherwise it goes at the end.

   If WARN is nonzero, we will warn if no file is found
   through this prefix.  WARN should point to an int
   which will be set to 1 if this entry is used.

   REQUIRE_MACHINE_SUFFIX is 1 if this prefix can't be used without
   the complete value of machine_suffix.
   2 means try both machine_suffix and just_machine_suffix.  */

static void
add_prefix (pprefix, prefix, first, require_machine_suffix, warn)
     struct path_prefix *pprefix;
     char *prefix;
     int first;
     int require_machine_suffix;
     int *warn;
{
  struct prefix_list *pl, **prev;
  int len;

  if (!first && pprefix->plist)
    {
      for (pl = pprefix->plist; pl->next; pl = pl->next)
	;
      prev = &pl->next;
    }
  else
    prev = &pprefix->plist;

  /* Keep track of the longest prefix */

  len = strlen (prefix);
  if (len > pprefix->max_len)
    pprefix->max_len = len;

  pl = (struct prefix_list *) xmalloc (sizeof (struct prefix_list));
  pl->prefix = save_string (prefix, len);
  pl->require_machine_suffix = require_machine_suffix;
  pl->used_flag_ptr = warn;
  if (warn)
    *warn = 0;

  if (*prev)
    pl->next = *prev;
  else
    pl->next = (struct prefix_list *) 0;
  *prev = pl;
}

/* Print warnings for any prefixes in the list PPREFIX that were not used.  */

static void
unused_prefix_warnings (pprefix)
     struct path_prefix *pprefix;
{
  struct prefix_list *pl = pprefix->plist;

  while (pl)
    {
      if (pl->used_flag_ptr != 0 && !*pl->used_flag_ptr)
	{
	  error ("file path prefix `%s' never used",
		 pl->prefix);
	  /* Prevent duplicate warnings.  */
	  *pl->used_flag_ptr = 1;
	}
      pl = pl->next;
    }
}

/* Get rid of all prefixes built up so far in *PLISTP. */

static void
free_path_prefix (pprefix)
     struct path_prefix *pprefix;
{
  struct prefix_list *pl = pprefix->plist;
  struct prefix_list *temp;

  while (pl)
    {
      temp = pl;
      pl = pl->next;
      free (temp->prefix);
      free ((char *) temp);
    }
  pprefix->plist = (struct prefix_list *) 0;
}

/* stdin file number.  */
#define STDIN_FILE_NO 0

/* stdout file number.  */
#define STDOUT_FILE_NO 1

/* value of `pipe': port index for reading.  */
#define READ_PORT 0

/* value of `pipe': port index for writing.  */
#define WRITE_PORT 1

/* Pipe waiting from last process, to be used as input for the next one.
   Value is STDIN_FILE_NO if no pipe is waiting
   (i.e. the next command is the first of a group).  */

static int last_pipe_input;

/* Fork one piped subcommand.  FUNC is the system call to use
   (either execv or execvp).  ARGV is the arg vector to use.
   NOT_LAST is nonzero if this is not the last subcommand
   (i.e. its output should be piped to the next one.)  */

#ifdef __MSDOS__

#include <process.h>
static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
#ifdef __GO32__
  int i = (search_flag ? spawnv : spawnvp) (1, program, argv);
#else
  char *scmd, *rf;
  FILE *argfile;
  int i, el = search_flag ? 0 : 4;

  scmd = (char *)malloc (strlen (program) + strlen (temp_filename) + 6 + el);
  rf = scmd + strlen(program) + 2 + el;
  sprintf (scmd, "%s%s @%s.gp", program,
	   (search_flag ? "" : ".exe"), temp_filename);
  argfile = fopen (rf, "w");
  if (argfile == 0)
    pfatal_with_name (rf);

  for (i=1; argv[i]; i++)
    {
      char *cp;
      for (cp = argv[i]; *cp; cp++)
	{
	  if (*cp == '"' || *cp == '\'' || *cp == '\\' || isspace (*cp))
	    fputc ('\\', argfile);
	  fputc (*cp, argfile);
	}
      fputc ('\n', argfile);
    }
  fclose (argfile);

  i = system (scmd);

  remove (rf);
#endif
  
  if (i == -1)
    {
      perror_exec (program);
      return MIN_FATAL_STATUS << 8;
    }
  return i << 8;
}

#endif

#if !defined(__MSDOS__) && !defined(OS2) && !defined(_WIN32)

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  int (*func)() = (search_flag ? execv : execvp);
  int pid;
  int pdes[2];
  int input_desc = last_pipe_input;
  int output_desc = STDOUT_FILE_NO;
  int retries, sleep_interval;

  /* If this isn't the last process, make a pipe for its output,
     and record it as waiting to be the input to the next process.  */

  if (not_last)
    {
      if (pipe (pdes) < 0)
	pfatal_with_name ("pipe");
      output_desc = pdes[WRITE_PORT];
      last_pipe_input = pdes[READ_PORT];
    }
  else
    last_pipe_input = STDIN_FILE_NO;

  /* Fork a subprocess; wait and retry if it fails.  */
  sleep_interval = 1;
  for (retries = 0; retries < 4; retries++)
    {
      pid = vfork ();
      if (pid >= 0)
	break;
      sleep (sleep_interval);
      sleep_interval *= 2;
    }

  switch (pid)
    {
    case -1:
#ifdef vfork
      pfatal_with_name ("fork");
#else
      pfatal_with_name ("vfork");
#endif
      /* NOTREACHED */
      return 0;

    case 0: /* child */
      /* Move the input and output pipes into place, if nec.  */
      if (input_desc != STDIN_FILE_NO)
	{
	  close (STDIN_FILE_NO);
	  dup (input_desc);
	  close (input_desc);
	}
      if (output_desc != STDOUT_FILE_NO)
	{
	  close (STDOUT_FILE_NO);
	  dup (output_desc);
	  close (output_desc);
	}

      /* Close the parent's descs that aren't wanted here.  */
      if (last_pipe_input != STDIN_FILE_NO)
	close (last_pipe_input);

      /* Exec the program.  */
      (*func) (program, argv);
      perror_exec (program);
      exit (-1);
      /* NOTREACHED */
      return 0;

    default:
      /* In the parent, after forking.
	 Close the descriptors that we made for this child.  */
      if (input_desc != STDIN_FILE_NO)
	close (input_desc);
      if (output_desc != STDOUT_FILE_NO)
	close (output_desc);

      /* Return child's process number.  */
      return pid;
    }
}

#endif /* not __MSDOS__ and not OS2 and not _WIN32 */

#if defined(OS2)

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  return (search_flag ? spawnv : spawnvp) (1, program, argv);
}
#endif /* OS2 */

#if defined(_WIN32)

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  return (search_flag ? __spawnv : __spawnvp) (1, program, argv);
}
#endif /* _WIN32 */


/* Execute the command specified by the arguments on the current line of spec.
   When using pipes, this includes several piped-together commands
   with `|' between them.

   Return 0 if successful, -1 if failed.  */

static int
execute ()
{
  int i;
  int n_commands;		/* # of command.  */
  char *string;
  struct command
    {
      char *prog;		/* program name.  */
      char **argv;		/* vector of args.  */
      int pid;			/* pid of process for this command.  */
    };

  struct command *commands;	/* each command buffer with above info.  */

  /* Count # of piped commands.  */
  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      n_commands++;

  /* Get storage for each command.  */
  commands
    = (struct command *) alloca (n_commands * sizeof (struct command));

  /* Split argbuf into its separate piped processes,
     and record info about each one.
     Also search for the programs that are to be run.  */

  commands[0].prog = argbuf[0]; /* first command.  */
  commands[0].argv = &argbuf[0];
  string = find_a_file (&exec_prefixes, commands[0].prog, X_OK);
  if (string)
    commands[0].argv[0] = string;

  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      {				/* each command.  */
#ifdef __MSDOS__
        fatal ("-pipe not supported under MS-DOS");
#endif
	argbuf[i] = 0;	/* termination of command args.  */
	commands[n_commands].prog = argbuf[i + 1];
	commands[n_commands].argv = &argbuf[i + 1];
	string = find_a_file (&exec_prefixes, commands[n_commands].prog, X_OK);
	if (string)
	  commands[n_commands].argv[0] = string;
	n_commands++;
      }

  argbuf[argbuf_index] = 0;

  /* If -v, print what we are about to do, and maybe query.  */

  if (verbose_flag)
    {
      /* Print each piped command as a separate line.  */
      for (i = 0; i < n_commands ; i++)
	{
	  char **j;

	  for (j = commands[i].argv; *j; j++)
	    fprintf (stderr, " %s", *j);

	  /* Print a pipe symbol after all but the last command.  */
	  if (i + 1 != n_commands)
	    fprintf (stderr, " |");
	  fprintf (stderr, "\n");
	}
      fflush (stderr);
#ifdef DEBUG
      fprintf (stderr, "\nGo ahead? (y or n) ");
      fflush (stderr);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;
      if (i != 'y' && i != 'Y')
	return 0;
#endif /* DEBUG */
    }

  /* Run each piped subprocess.  */

  last_pipe_input = STDIN_FILE_NO;
  for (i = 0; i < n_commands; i++)
    {
      char *string = commands[i].argv[0];

      commands[i].pid = pexecute (string != commands[i].prog,
				  string, commands[i].argv,
				  i + 1 < n_commands);

      if (string != commands[i].prog)
	free (string);
    }

  execution_count++;

  /* Wait for all the subprocesses to finish.
     We don't care what order they finish in;
     we know that N_COMMANDS waits will get them all.
     Ignore subprocesses that we don't know about,
     since they can be spawned by the process that exec'ed us.  */

  {
    int ret_code = 0;

    for (i = 0; i < n_commands; )
      {
	int j;
	int status;
	int pid;

#ifdef __MSDOS__
        status = pid = commands[i].pid;
#else
#ifdef _WIN32
	pid = cwait (&status, commands[i].pid, WAIT_CHILD);
#else
	pid = wait (&status);
#endif
#endif
	if (pid < 0)
	  abort ();

	for (j = 0; j < n_commands; j++)
	  if (commands[j].pid == pid)
	    {
	      i++;
	      if (status != 0)
		{
		  if (WIFSIGNALED (status))
		    {
		      fatal ("Internal compiler error: program %s got fatal signal %d",
			     commands[j].prog, WTERMSIG (status));
		      signal_count++;
		      ret_code = -1;
		    }
		  else if (WIFEXITED (status)
			   && WEXITSTATUS (status) >= MIN_FATAL_STATUS)
		    ret_code = -1;
		}
	      break;
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
   The `live_cond' field is 1 if the switch is true in a conditional spec,
   -1 if false (overridden by a later switch), and is initialized to zero.
   The `valid' field is nonzero if any spec has looked at this switch;
   if it remains zero at the end of the run, it must be meaningless.  */

struct switchstr
{
  char *part1;
  char **args;
  int live_cond;
  int valid;
};

static struct switchstr *switches;

static int n_switches;

struct infile
{
  char *name;
  char *language;
};

/* Also a vector of input files specified.  */

static struct infile *infiles;

static int n_infiles;

/* And a vector of corresponding output files is made up later.  */

static char **outfiles;

/* Create the vector `switches' and its contents.
   Store its length in `n_switches'.  */

static void
process_command (argc, argv)
     int argc;
     char **argv;
{
  register int i;
  char *temp;
  char *spec_lang = 0;
  int last_language_n_infiles;

  gcc_exec_prefix = getenv ("GCC_EXEC_PREFIX");

  n_switches = 0;
  n_infiles = 0;

  /* Figure compiler version from version string.  */

  compiler_version = save_string (version_string, strlen (version_string));
  for (temp = compiler_version; *temp; ++temp)
    {
      if (*temp == ' ')
	{
	  *temp = '\0';
	  break;
	}
    }

  /* Set up the default search paths.  */

  if (gcc_exec_prefix)
    {
      add_prefix (&exec_prefixes, gcc_exec_prefix, 0, 0, NULL_PTR);
      add_prefix (&startfile_prefixes, gcc_exec_prefix, 0, 0, NULL_PTR);
    }

  /* COMPILER_PATH and LIBRARY_PATH have values
     that are lists of directory names with colons.  */

  temp = getenv ("COMPILER_PATH");
  if (temp)
    {
      char *startp, *endp;
      char *nstore = (char *) alloca (strlen (temp) + 3);

      startp = endp = temp;
      while (1)
	{
	  if (*endp == PATH_SEPARATOR || *endp == 0)
	    {
	      strncpy (nstore, startp, endp-startp);
	      if (endp == startp)
		strcpy (nstore, concat (".", dir_separator_str));
	      else if (endp[-1] != '/' && endp[-1] != DIR_SEPARATOR)
		{
		  nstore[endp-startp] = DIR_SEPARATOR;
		  nstore[endp-startp+1] = 0;
		}
	      else
		nstore[endp-startp] = 0;
	      add_prefix (&exec_prefixes, nstore, 0, 0, NULL_PTR);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  temp = getenv ("LIBRARY_PATH");
  if (temp && ! cross_compile)
    {
      char *startp, *endp;
      char *nstore = (char *) alloca (strlen (temp) + 3);

      startp = endp = temp;
      while (1)
	{
	  if (*endp == PATH_SEPARATOR || *endp == 0)
	    {
	      strncpy (nstore, startp, endp-startp);
	      if (endp == startp)
		strcpy (nstore, concat (".", dir_separator_str));
	      else if (endp[-1] != '/' && endp[-1] != DIR_SEPARATOR)
		{
		  nstore[endp-startp] = DIR_SEPARATOR;
		  nstore[endp-startp+1] = 0;
		}
	      else
		nstore[endp-startp] = 0;
	      add_prefix (&startfile_prefixes, nstore, 0, 0, NULL_PTR);
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
  if (temp && ! cross_compile)
    {
      char *startp, *endp;
      char *nstore = (char *) alloca (strlen (temp) + 3);

      startp = endp = temp;
      while (1)
	{
	  if (*endp == PATH_SEPARATOR || *endp == 0)
	    {
	      strncpy (nstore, startp, endp-startp);
	      if (endp == startp)
		strcpy (nstore, concat (".", dir_separator_str));
	      else if (endp[-1] != '/' && endp[-1] != DIR_SEPARATOR)
		{
		  nstore[endp-startp] = DIR_SEPARATOR;
		  nstore[endp-startp+1] = 0;
		}
	      else
		nstore[endp-startp] = 0;
	      add_prefix (&startfile_prefixes, nstore, 0, 0, NULL_PTR);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  /* Convert new-style -- options to old-style.  */
  translate_options (&argc, &argv);

  /* Scan argv twice.  Here, the first time, just count how many switches
     there will be in their vector, and how many input files in theirs.
     Here we also parse the switches that cc itself uses (e.g. -v).  */

  for (i = 1; i < argc; i++)
    {
      if (! strcmp (argv[i], "-dumpspecs"))
	{
	  printf ("*asm:\n%s\n\n", asm_spec);
	  printf ("*asm_final:\n%s\n\n", asm_final_spec);
	  printf ("*cpp:\n%s\n\n", cpp_spec);
	  printf ("*cc1:\n%s\n\n", cc1_spec);
	  printf ("*cc1plus:\n%s\n\n", cc1plus_spec);
	  printf ("*endfile:\n%s\n\n", endfile_spec);
	  printf ("*link:\n%s\n\n", link_spec);
	  printf ("*lib:\n%s\n\n", lib_spec);
	  printf ("*libgcc:\n%s\n\n", libgcc_spec);
	  printf ("*startfile:\n%s\n\n", startfile_spec);
	  printf ("*switches_need_spaces:\n%s\n\n", switches_need_spaces);
	  printf ("*signed_char:\n%s\n\n", signed_char_spec);
	  printf ("*predefines:\n%s\n\n", cpp_predefines);
	  printf ("*cross_compile:\n%d\n\n", cross_compile);
	  printf ("*multilib:\n%s\n\n", multilib_select);

	  exit (0);
	}
      else if (! strcmp (argv[i], "-dumpversion"))
	{
	  printf ("%s\n", version_string);
	  exit (0);
	}
      else if (! strcmp (argv[i], "-dumpmachine"))
	{
	  printf ("%s\n", spec_machine);
	  exit  (0);
	}
      else if (! strcmp (argv[i], "-print-search-dirs"))
	print_search_dirs = 1;
      else if (! strcmp (argv[i], "-print-libgcc-file-name"))
	print_file_name = "libgcc.a";
      else if (! strncmp (argv[i], "-print-file-name=", 17))
	print_file_name = argv[i] + 17;
      else if (! strncmp (argv[i], "-print-prog-name=", 17))
	print_prog_name = argv[i] + 17;
      else if (! strcmp (argv[i], "-print-multi-lib"))
	print_multi_lib = 1;
      else if (! strcmp (argv[i], "-print-multi-directory"))
	print_multi_directory = 1;
      else if (! strncmp (argv[i], "-Wa,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the assembler.  */

	  n_assembler_options++;
	  if (!assembler_options)
	    assembler_options
	      = (char **) xmalloc (n_assembler_options * sizeof (char **));
	  else
	    assembler_options
	      = (char **) xrealloc (assembler_options,
				    n_assembler_options * sizeof (char **));

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		assembler_options[n_assembler_options - 1]
		  = save_string (argv[i] + prev, j - prev);
		n_assembler_options++;
		assembler_options
		  = (char **) xrealloc (assembler_options,
					n_assembler_options * sizeof (char **));
		prev = j + 1;
	      }
	  /* Record the part after the last comma.  */
	  assembler_options[n_assembler_options - 1] = argv[i] + prev;
	}
      else if (! strncmp (argv[i], "-Wp,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the preprocessor.  */

	  n_preprocessor_options++;
	  if (!preprocessor_options)
	    preprocessor_options
	      = (char **) xmalloc (n_preprocessor_options * sizeof (char **));
	  else
	    preprocessor_options
	      = (char **) xrealloc (preprocessor_options,
				    n_preprocessor_options * sizeof (char **));

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		preprocessor_options[n_preprocessor_options - 1]
		  = save_string (argv[i] + prev, j - prev);
		n_preprocessor_options++;
		preprocessor_options
		  = (char **) xrealloc (preprocessor_options,
					n_preprocessor_options * sizeof (char **));
		prev = j + 1;
	      }
	  /* Record the part after the last comma.  */
	  preprocessor_options[n_preprocessor_options - 1] = argv[i] + prev;
	}
      else if (argv[i][0] == '+' && argv[i][1] == 'e')
	/* The +e options to the C++ front-end.  */
	n_switches++;
      else if (strncmp (argv[i], "-Wl,", 4) == 0)
	{
	  int j;
	  /* Split the argument at commas.  */
	  for (j = 3; argv[i][j]; j++)
	    n_infiles += (argv[i][j] == ',');
	}
      else if (strcmp (argv[i], "-Xlinker") == 0)
	{
	  if (i + 1 == argc)
	    fatal ("argument to `-Xlinker' is missing");

	  n_infiles++;
	  i++;
	}
      else if (strncmp (argv[i], "-l", 2) == 0)
	n_infiles++;
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  register char *p = &argv[i][1];
	  register int c = *p;

	  switch (c)
	    {
	    case 'b':
	      if (p[1] == 0 && i + 1 == argc)
		fatal ("argument to `-b' is missing");
	      if (p[1] == 0)
		spec_machine = argv[++i];
	      else
		spec_machine = p + 1;
	      break;

	    case 'B':
	      {
		int *temp = (int *) xmalloc (sizeof (int));
		char *value;
		if (p[1] == 0 && i + 1 == argc)
		  fatal ("argument to `-B' is missing");
		if (p[1] == 0)
		  value = argv[++i];
		else
		  value = p + 1;
		add_prefix (&exec_prefixes, value, 1, 0, temp);
		add_prefix (&startfile_prefixes, value, 1, 0, temp);
		add_prefix (&include_prefixes, concat (value, "include"),
			    1, 0, 0);

		/* As a kludge, if the arg is "[foo/]stageN/", just add
		   "[foo/]include" to the include prefix.  */
		{
		  int len = strlen (value);
		  if ((len == 7
		       || (len > 7
			   && (value[len - 8] == '/'
			       || value[len - 8] == DIR_SEPARATOR)))
		      && strncmp (value + len - 7, "stage", 5) == 0
		      && isdigit (value[len - 2])
		      && (value[len - 1] == '/'
			  || value[len - 1] == DIR_SEPARATOR))
		    {
		      if (len == 7)
			add_prefix (&include_prefixes, "include", 1, 0, 0);
		      else
			{
			  char *string = xmalloc (len + 1);
			  strncpy (string, value, len-7);
			  strcat (string, "include");
			  add_prefix (&include_prefixes, string, 1, 0, 0);
			}
		    }
		}
	      }
	      break;

	    case 'v':	/* Print our subcommands and print versions.  */
	      n_switches++;
	      /* If they do anything other than exactly `-v', don't set
		 verbose_flag; rather, continue on to give the error.  */
	      if (p[1] != 0)
		break;
	      verbose_flag++;
	      break;

	    case 'V':
	      if (p[1] == 0 && i + 1 == argc)
		fatal ("argument to `-V' is missing");
	      if (p[1] == 0)
		spec_version = argv[++i];
	      else
		spec_version = p + 1;
	      compiler_version = spec_version;
	      break;

	    case 's':
	      if (!strcmp (p, "save-temps"))
		{
		  save_temps_flag = 1;
		  n_switches++;
		  break;
		}
	    default:
	      n_switches++;

	      if (SWITCH_TAKES_ARG (c) > (p[1] != 0))
		i += SWITCH_TAKES_ARG (c) - (p[1] != 0);
	      else if (WORD_SWITCH_TAKES_ARG (p))
		i += WORD_SWITCH_TAKES_ARG (p);
	    }
	}
      else
	n_infiles++;
    }

  /* Set up the search paths before we go looking for config files.  */

  /* These come before the md prefixes so that we will find gcc's subcommands
     (such as cpp) rather than those of the host system.  */
  /* Use 2 as fourth arg meaning try just the machine as a suffix,
     as well as trying the machine and the version.  */
#ifndef OS2
  add_prefix (&exec_prefixes, standard_exec_prefix, 0, 2, NULL_PTR);
  add_prefix (&exec_prefixes, standard_exec_prefix_1, 0, 2, NULL_PTR);
#endif

  add_prefix (&startfile_prefixes, standard_exec_prefix, 0, 1, NULL_PTR);
  add_prefix (&startfile_prefixes, standard_exec_prefix_1, 0, 1, NULL_PTR);

  tooldir_prefix = concat3 (tooldir_base_prefix, spec_machine, 
                            dir_separator_str);

  /* If tooldir is relative, base it on exec_prefixes.  A relative
     tooldir lets us move the installed tree as a unit.

     If GCC_EXEC_PREFIX is defined, then we want to add two relative
     directories, so that we can search both the user specified directory
     and the standard place.  */

  if (*tooldir_prefix != '/' && *tooldir_prefix != DIR_SEPARATOR)
    {
      if (gcc_exec_prefix)
	{
	  char *gcc_exec_tooldir_prefix
	    = concat6 (gcc_exec_prefix, spec_machine, dir_separator_str,
		      spec_version, dir_separator_str, tooldir_prefix);

	  add_prefix (&exec_prefixes,
		      concat3 (gcc_exec_tooldir_prefix, "bin", 
                               dir_separator_str),
		      0, 0, NULL_PTR);
	  add_prefix (&startfile_prefixes,
		      concat3 (gcc_exec_tooldir_prefix, "lib", 
                               dir_separator_str),
		      0, 0, NULL_PTR);
	}

      tooldir_prefix = concat6 (standard_exec_prefix, spec_machine,
			        dir_separator_str, spec_version, 
                                dir_separator_str, tooldir_prefix);
    }

  add_prefix (&exec_prefixes, 
              concat3 (tooldir_prefix, "bin", dir_separator_str),
	      0, 0, NULL_PTR);
  add_prefix (&startfile_prefixes,
	      concat3 (tooldir_prefix, "lib", dir_separator_str),
	      0, 0, NULL_PTR);

  /* More prefixes are enabled in main, after we read the specs file
     and determine whether this is cross-compilation or not.  */


  /* Then create the space for the vectors and scan again.  */

  switches = ((struct switchstr *)
	      xmalloc ((n_switches + 1) * sizeof (struct switchstr)));
  infiles = (struct infile *) xmalloc ((n_infiles + 1) * sizeof (struct infile));
  n_switches = 0;
  n_infiles = 0;
  last_language_n_infiles = -1;

  /* This, time, copy the text of each switch and store a pointer
     to the copy in the vector of switches.
     Store all the infiles in their vector.  */

  for (i = 1; i < argc; i++)
    {
      /* Just skip the switches that were handled by the preceding loop.  */
      if (! strncmp (argv[i], "-Wa,", 4))
	;
      else if (! strncmp (argv[i], "-Wp,", 4))
	;
      else if (! strcmp (argv[i], "-print-search-dirs"))
	;
      else if (! strcmp (argv[i], "-print-libgcc-file-name"))
	;
      else if (! strncmp (argv[i], "-print-file-name=", 17))
	;
      else if (! strncmp (argv[i], "-print-prog-name=", 17))
	;
      else if (! strcmp (argv[i], "-print-multi-lib"))
	;
      else if (! strcmp (argv[i], "-print-multi-directory"))
	;
      else if (argv[i][0] == '+' && argv[i][1] == 'e')
	{
	  /* Compensate for the +e options to the C++ front-end;
	     they're there simply for cfront call-compatibility.  We do
	     some magic in default_compilers to pass them down properly.
	     Note we deliberately start at the `+' here, to avoid passing
	     -e0 or -e1 down into the linker.  */
	  switches[n_switches].part1 = &argv[i][0];
	  switches[n_switches].args = 0;
	  switches[n_switches].live_cond = 0;
	  switches[n_switches].valid = 0;
	  n_switches++;
	}
      else if (strncmp (argv[i], "-Wl,", 4) == 0)
	{
	  int prev, j;
	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		infiles[n_infiles].language = 0;
		infiles[n_infiles++].name
		  = save_string (argv[i] + prev, j - prev);
		prev = j + 1;
	      }
	  /* Record the part after the last comma.  */
	  infiles[n_infiles].language = 0;
	  infiles[n_infiles++].name = argv[i] + prev;
	}
      else if (strcmp (argv[i], "-Xlinker") == 0)
	{
	  infiles[n_infiles].language = 0;
	  infiles[n_infiles++].name = argv[++i];
	}
      else if (strncmp (argv[i], "-l", 2) == 0)
	{
	  infiles[n_infiles].language = 0;
	  infiles[n_infiles++].name = argv[i];
	}
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  register char *p = &argv[i][1];
	  register int c = *p;

	  if (c == 'B' || c == 'b' || c == 'V')
	    {
	      /* Skip a separate arg, if any.  */
	      if (p[1] == 0)
		i++;
	      continue;
	    }
	  if (c == 'x')
	    {
	      if (p[1] == 0 && i + 1 == argc)
		fatal ("argument to `-x' is missing");
	      if (p[1] == 0)
		spec_lang = argv[++i];
	      else
		spec_lang = p + 1;
	      if (! strcmp (spec_lang, "none"))
		/* Suppress the warning if -xnone comes after the last input
		   file, because alternate command interfaces like g++ might
		   find it useful to place -xnone after each input file.  */
		spec_lang = 0;
	      else
		last_language_n_infiles = n_infiles;
	      continue;
	    }
	  switches[n_switches].part1 = p;
	  /* Deal with option arguments in separate argv elements.  */
	  if ((SWITCH_TAKES_ARG (c) > (p[1] != 0))
	      || WORD_SWITCH_TAKES_ARG (p))
	    {
	      int j = 0;
	      int n_args = WORD_SWITCH_TAKES_ARG (p);

	      if (n_args == 0)
		{
		  /* Count only the option arguments in separate argv elements.  */
		  n_args = SWITCH_TAKES_ARG (c) - (p[1] != 0);
		}
	      if (i + n_args >= argc)
		fatal ("argument to `-%s' is missing", p);
	      switches[n_switches].args
		= (char **) xmalloc ((n_args + 1) * sizeof (char *));
	      while (j < n_args)
		switches[n_switches].args[j++] = argv[++i];
	      /* Null-terminate the vector.  */
	      switches[n_switches].args[j] = 0;
	    }
	  else if (*switches_need_spaces != 0 && (c == 'o' || c == 'L'))
	    {
	      /* On some systems, ld cannot handle -o or -L without space.
		 So split the -o or -L from its argument.  */
	      switches[n_switches].part1 = (c == 'o' ? "o" : "L");
	      switches[n_switches].args = (char **) xmalloc (2 * sizeof (char *));
	      switches[n_switches].args[0] = xmalloc (strlen (p));
	      strcpy (switches[n_switches].args[0], &p[1]);
	      switches[n_switches].args[1] = 0;
	    }
	  else
	    switches[n_switches].args = 0;

	  switches[n_switches].live_cond = 0;
	  switches[n_switches].valid = 0;
	  /* This is always valid, since gcc.c itself understands it.  */
	  if (!strcmp (p, "save-temps"))
	    switches[n_switches].valid = 1;
	  n_switches++;
	}
      else
	{
#ifdef HAVE_OBJECT_SUFFIX
	  /* Convert x.o to x.obj if OBJECT_SUFFIX is ".obj".  */
	  if (strlen (argv[i]) > 2
	      && argv[i][strlen (argv[i]) - 2] == '.'
	      && argv[i][strlen (argv[i]) - 1] == 'o')
	    {
	      int j;

	      for (j = 0; j < strlen (argv[i]) - 2; j++)
		obstack_1grow (&obstack, argv[i][j]);

	      obstack_grow (&obstack, OBJECT_SUFFIX, strlen (OBJECT_SUFFIX));
	      obstack_1grow (&obstack, 0);
	      argv[i] = obstack_finish (&obstack);
	    }
#endif

	  if (strcmp (argv[i], "-") != 0 && access (argv[i], R_OK) < 0)
	    {
	      perror_with_name (argv[i]);
	      error_count++;
	    }
	  else
	    {
	      infiles[n_infiles].language = spec_lang;
	      infiles[n_infiles++].name = argv[i];
	    }
	}
    }

  if (n_infiles == last_language_n_infiles && spec_lang != 0)
    error ("Warning: `-x %s' after last input file has no effect", spec_lang);

  switches[n_switches].part1 = 0;
  infiles[n_infiles].name = 0;
}

/* Process a spec string, accumulating and running commands.  */

/* These variables describe the input file name.
   input_file_number is the index on outfiles of this file,
   so that the output file name can be stored for later use by %o.
   input_basename is the start of the part of the input file
   sans all directory names, and basename_length is the number
   of characters starting there excluding the suffix .c or whatever.  */

static char *input_filename;
static int input_file_number;
static int input_filename_length;
static int basename_length;
static char *input_basename;
static char *input_suffix;

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

/* Nonzero means that the input of this command is coming from a pipe.  */
static int input_from_pipe;

/* Process the spec SPEC and run the commands specified therein.
   Returns 0 if the spec is successfully processed; -1 if failed.  */

static int
do_spec (spec)
     char *spec;
{
  int value;

  clear_args ();
  arg_going = 0;
  delete_this_arg = 0;
  this_is_output_file = 0;
  this_is_library_file = 0;
  input_from_pipe = 0;

  value = do_spec_1 (spec, 0, NULL_PTR);

  /* Force out any unfinished command.
     If -pipe, this forces out the last command if it ended in `|'.  */
  if (value == 0)
    {
      if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	argbuf_index--;

      if (argbuf_index > 0)
	value = execute ();
    }

  return value;
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
do_spec_1 (spec, inswitch, soft_matched_part)
     char *spec;
     int inswitch;
     char *soft_matched_part;
{
  register char *p = spec;
  register int c;
  int i;
  char *string;
  int value;

  while (c = *p++)
    /* If substituting a switch, treat all chars like letters.
       Otherwise, NL, SPC, TAB and % are special.  */
    switch (inswitch ? 'a' : c)
      {
      case '\n':
	/* End of line: finish any pending argument,
	   then run the pending command if one has been started.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    if (this_is_library_file)
	      string = find_file (string);
	    store_arg (string, delete_this_arg, this_is_output_file);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }
	arg_going = 0;

	if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	  {
	    for (i = 0; i < n_switches; i++)
	      if (!strcmp (switches[i].part1, "pipe"))
		break;

	    /* A `|' before the newline means use a pipe here,
	       but only if -pipe was specified.
	       Otherwise, execute now and don't pass the `|' as an arg.  */
	    if (i < n_switches)
	      {
		input_from_pipe = 1;
		switches[i].valid = 1;
		break;
	      }
	    else
	      argbuf_index--;
	  }

	if (argbuf_index > 0)
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
	input_from_pipe = 0;
	break;

      case '|':
	/* End any pending argument.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    if (this_is_library_file)
	      string = find_file (string);
	    store_arg (string, delete_this_arg, this_is_output_file);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }

	/* Use pipe */
	obstack_1grow (&obstack, c);
	arg_going = 1;
	break;

      case '\t':
      case ' ':
	/* Space or tab ends an argument if one is pending.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    if (this_is_library_file)
	      string = find_file (string);
	    store_arg (string, delete_this_arg, this_is_output_file);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }
	/* Reinitialize for a new argument.  */
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
	break;

      case '%':
	switch (c = *p++)
	  {
	  case 0:
	    fatal ("Invalid specification!  Bug in cc.");

	  case 'b':
	    obstack_grow (&obstack, input_basename, basename_length);
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
	      struct prefix_list *pl = startfile_prefixes.plist;
	      int bufsize = 100;
	      char *buffer = (char *) xmalloc (bufsize);
	      int idx;

	      for (; pl; pl = pl->next)
		{
#ifdef RELATIVE_PREFIX_NOT_LINKDIR
		  /* Used on systems which record the specified -L dirs
		     and use them to search for dynamic linking.  */
		  /* Relative directories always come from -B,
		     and it is better not to use them for searching
		     at run time.  In particular, stage1 loses  */
		  if (pl->prefix[0] != '/' && pl->prefix[0] != DIR_SEPARATOR)
		    continue;
#endif
		  /* Try subdirectory if there is one.  */
		  if (multilib_dir != NULL)
		    {
		      if (machine_suffix)
			{
			  if (strlen (pl->prefix) + strlen (machine_suffix)
			      >= bufsize)
			    bufsize = (strlen (pl->prefix)
				       + strlen (machine_suffix)) * 2 + 1;
			  buffer = (char *) xrealloc (buffer, bufsize);
			  strcpy (buffer, pl->prefix);
			  strcat (buffer, machine_suffix);
			  if (is_directory (buffer, multilib_dir, 1))
			    {
			      do_spec_1 ("-L", 0, NULL_PTR);
#ifdef SPACE_AFTER_L_OPTION
			      do_spec_1 (" ", 0, NULL_PTR);
#endif
			      do_spec_1 (buffer, 1, NULL_PTR);
			      do_spec_1 (multilib_dir, 1, NULL_PTR);
			      /* Make this a separate argument.  */
			      do_spec_1 (" ", 0, NULL_PTR);
			    }
			}
		      if (!pl->require_machine_suffix)
			{
			  if (is_directory (pl->prefix, multilib_dir, 1))
			    {
			      do_spec_1 ("-L", 0, NULL_PTR);
#ifdef SPACE_AFTER_L_OPTION
			      do_spec_1 (" ", 0, NULL_PTR);
#endif
			      do_spec_1 (pl->prefix, 1, NULL_PTR);
			      do_spec_1 (multilib_dir, 1, NULL_PTR);
			      /* Make this a separate argument.  */
			      do_spec_1 (" ", 0, NULL_PTR);
			    }
			}
		    }
		  if (machine_suffix)
		    {
		      if (is_directory (pl->prefix, machine_suffix, 1))
			{
			  do_spec_1 ("-L", 0, NULL_PTR);
#ifdef SPACE_AFTER_L_OPTION
			  do_spec_1 (" ", 0, NULL_PTR);
#endif
			  do_spec_1 (pl->prefix, 1, NULL_PTR);
			  /* Remove slash from machine_suffix.  */
			  if (strlen (machine_suffix) >= bufsize)
			    bufsize = strlen (machine_suffix) * 2 + 1;
			  buffer = (char *) xrealloc (buffer, bufsize);
			  strcpy (buffer, machine_suffix);
			  idx = strlen (buffer);
			  if (buffer[idx - 1] == '/'
			      || buffer[idx - 1] == DIR_SEPARATOR)
			    buffer[idx - 1] = 0;
			  do_spec_1 (buffer, 1, NULL_PTR);
			  /* Make this a separate argument.  */
			  do_spec_1 (" ", 0, NULL_PTR);
			}
		    }
		  if (!pl->require_machine_suffix)
		    {
		      if (is_directory (pl->prefix, "", 1))
			{
			  do_spec_1 ("-L", 0, NULL_PTR);
#ifdef SPACE_AFTER_L_OPTION
			  do_spec_1 (" ", 0, NULL_PTR);
#endif
			  /* Remove slash from pl->prefix.  */
			  if (strlen (pl->prefix) >= bufsize)
			    bufsize = strlen (pl->prefix) * 2 + 1;
			  buffer = (char *) xrealloc (buffer, bufsize);
			  strcpy (buffer, pl->prefix);
			  idx = strlen (buffer);
			  if (buffer[idx - 1] == '/'
			      || buffer[idx - 1] == DIR_SEPARATOR)
			    buffer[idx - 1] = 0;
			  do_spec_1 (buffer, 1, NULL_PTR);
			  /* Make this a separate argument.  */
			  do_spec_1 (" ", 0, NULL_PTR);
			}
		    }
		}
	      free (buffer);
	    }
	    break;

	  case 'e':
	    /* {...:%efoo} means report an error with `foo' as error message
	       and don't execute any more commands for this file.  */
	    {
	      char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n') p++;
	      buf = (char *) alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      error ("%s", buf);
	      return -1;
	    }
	    break;

	  case 'g':
	  case 'u':
	  case 'U':
	    if (save_temps_flag)
	      {
		obstack_grow (&obstack, input_basename, basename_length);
		delete_this_arg = 0;
	      }
	    else
	      {
#ifdef MKTEMP_EACH_FILE
		/* ??? This has a problem: the total number of
		   values mktemp can return is limited.
		   That matters for the names of object files.
		   In 2.4, do something about that.  */
		struct temp_name *t;
		char *suffix = p;
		while (*p == '.' || isalpha (*p)
		       || (p[0] == '%' && p[1] == 'O'))
		  p++;

		/* See if we already have an association of %g/%u/%U and
		   suffix.  */
		for (t = temp_names; t; t = t->next)
		  if (t->length == p - suffix
		      && strncmp (t->suffix, suffix, p - suffix) == 0
		      && t->unique == (c != 'g'))
		    break;

		/* Make a new association if needed.  %u requires one.  */
		if (t == 0 || c == 'u')
		  {
		    if (t == 0)
		      {
			t = (struct temp_name *) xmalloc (sizeof (struct temp_name));
			t->next = temp_names;
			temp_names = t;
		      }
		    t->length = p - suffix;
		    t->suffix = save_string (suffix, p - suffix);
		    t->unique = (c != 'g');
		    choose_temp_base ();
		    t->filename = temp_filename;
		    t->filename_length = temp_filename_length;
		  }

		obstack_grow (&obstack, t->filename, t->filename_length);
		delete_this_arg = 1;
#else
		obstack_grow (&obstack, temp_filename, temp_filename_length);
		if (c == 'u' || c == 'U')
		  {
		    static int unique;
		    char buff[9];
		    if (c == 'u')
		      unique++;
		    sprintf (buff, "%d", unique);
		    obstack_grow (&obstack, buff, strlen (buff));
		  }
#endif
		delete_this_arg = 1;
	      }
	    arg_going = 1;
	    break;

	  case 'i':
	    obstack_grow (&obstack, input_filename, input_filename_length);
	    arg_going = 1;
	    break;

	  case 'I':
	    {
	      struct prefix_list *pl = include_prefixes.plist;

	      if (gcc_exec_prefix)
		{
		  do_spec_1 ("-iprefix", 1, NULL_PTR);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL_PTR);
		  do_spec_1 (gcc_exec_prefix, 1, NULL_PTR);
		  do_spec_1 (" ", 0, NULL_PTR);
		}

	      for (; pl; pl = pl->next)
		{
		  do_spec_1 ("-isystem", 1, NULL_PTR);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL_PTR);
		  do_spec_1 (pl->prefix, 1, NULL_PTR);
		  do_spec_1 (" ", 0, NULL_PTR);
		}
	    }
	    break;

	  case 'o':
	    for (i = 0; i < n_infiles; i++)
	      store_arg (outfiles[i], 0, 0);
	    break;

	  case 'O':
	    obstack_grow (&obstack, OBJECT_SUFFIX, strlen (OBJECT_SUFFIX));
	    arg_going = 1;
	    break;

	  case 's':
	    this_is_library_file = 1;
	    break;

	  case 'w':
	    this_is_output_file = 1;
	    break;

	  case 'W':
	    {
	      int index = argbuf_index;
	      /* Handle the {...} following the %W.  */
	      if (*p != '{')
		abort ();
	      p = handle_braces (p + 1);
	      if (p == 0)
		return -1;
	      /* If any args were output, mark the last one for deletion
		 on failure.  */
	      if (argbuf_index != index)
		record_temp_file (argbuf[argbuf_index - 1], 0, 1);
	      break;
	    }

	  /* %x{OPTION} records OPTION for %X to output.  */
	  case 'x':
	    {
	      char *p1 = p;
	      char *string;

	      /* Skip past the option value and make a copy.  */
	      if (*p != '{')
		abort ();
	      while (*p++ != '}')
		;
	      string = save_string (p1 + 1, p - p1 - 2);

	      /* See if we already recorded this option.  */
	      for (i = 0; i < n_linker_options; i++)
		if (! strcmp (string, linker_options[i]))
		  {
		    free (string);
		    return 0;
		  }

	      /* This option is new; add it.  */
	      n_linker_options++;
	      if (!linker_options)
		linker_options
		  = (char **) xmalloc (n_linker_options * sizeof (char **));
	      else
		linker_options
		  = (char **) xrealloc (linker_options,
					n_linker_options * sizeof (char **));

	      linker_options[n_linker_options - 1] = string;
	    }
	    break;

	  /* Dump out the options accumulated previously using %x.  */
	  case 'X':
	    for (i = 0; i < n_linker_options; i++)
	      {
		do_spec_1 (linker_options[i], 1, NULL_PTR);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL_PTR);
	      }
	    break;

	  /* Dump out the options accumulated previously using -Wa,.  */
	  case 'Y':
	    for (i = 0; i < n_assembler_options; i++)
	      {
		do_spec_1 (assembler_options[i], 1, NULL_PTR);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL_PTR);
	      }
	    break;

	  /* Dump out the options accumulated previously using -Wp,.  */
	  case 'Z':
	    for (i = 0; i < n_preprocessor_options; i++)
	      {
		do_spec_1 (preprocessor_options[i], 1, NULL_PTR);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL_PTR);
	      }
	    break;

	    /* Here are digits and numbers that just process
	       a certain constant string as a spec.  */

	  case '1':
	    value = do_spec_1 (cc1_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case '2':
	    value = do_spec_1 (cc1plus_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'a':
	    value = do_spec_1 (asm_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'A':
	    value = do_spec_1 (asm_final_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'c':
	    value = do_spec_1 (signed_char_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'C':
	    value = do_spec_1 (cpp_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'E':
	    value = do_spec_1 (endfile_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'l':
	    value = do_spec_1 (link_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'L':
	    value = do_spec_1 (lib_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'G':
	    value = do_spec_1 (libgcc_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	  case 'p':
	    {
	      char *x = (char *) alloca (strlen (cpp_predefines) + 1);
	      char *buf = x;
	      char *y;

	      /* Copy all of the -D options in CPP_PREDEFINES into BUF.  */
	      y = cpp_predefines;
	      while (*y != 0)
		{
		  if (! strncmp (y, "-D", 2))
		    /* Copy the whole option.  */
		    while (*y && *y != ' ' && *y != '\t')
		      *x++ = *y++;
		  else if (*y == ' ' || *y == '\t')
		    /* Copy whitespace to the result.  */
		    *x++ = *y++;
		  /* Don't copy other options.  */
		  else
		    y++;
		}

	      *x = 0;

	      value = do_spec_1 (buf, 0, NULL_PTR);
	      if (value != 0)
		return value;
	    }
	    break;

	  case 'P':
	    {
	      char *x = (char *) alloca (strlen (cpp_predefines) * 4 + 1);
	      char *buf = x;
	      char *y;

	      /* Copy all of CPP_PREDEFINES into BUF,
		 but put __ after every -D and at the end of each arg.  */
	      y = cpp_predefines;
	      while (*y != 0)
		{
		  if (! strncmp (y, "-D", 2))
		    {
		      int flag = 0;

		      *x++ = *y++;
		      *x++ = *y++;

		      if (*y != '_'
			  || (*(y+1) != '_' && ! isupper (*(y+1))))
		        {
			  /* Stick __ at front of macro name.  */
			  *x++ = '_';
			  *x++ = '_';
			  /* Arrange to stick __ at the end as well.  */
			  flag = 1;
			}

		      /* Copy the macro name.  */
		      while (*y && *y != '=' && *y != ' ' && *y != '\t')
			*x++ = *y++;

		      if (flag)
		        {
			  *x++ = '_';
			  *x++ = '_';
			}

		      /* Copy the value given, if any.  */
		      while (*y && *y != ' ' && *y != '\t')
			*x++ = *y++;
		    }
		  else if (*y == ' ' || *y == '\t')
		    /* Copy whitespace to the result.  */
		    *x++ = *y++;
		  /* Don't copy -A options  */
		  else
		    y++;
		}
	      *x++ = ' ';

	      /* Copy all of CPP_PREDEFINES into BUF,
		 but put __ after every -D.  */
	      y = cpp_predefines;
	      while (*y != 0)
		{
		  if (! strncmp (y, "-D", 2))
		    {
		      y += 2;

		      if (*y != '_'
			  || (*(y+1) != '_' && ! isupper (*(y+1))))
		        {
			  /* Stick -D__ at front of macro name.  */
			  *x++ = '-';
			  *x++ = 'D';
			  *x++ = '_';
			  *x++ = '_';

			  /* Copy the macro name.  */
			  while (*y && *y != '=' && *y != ' ' && *y != '\t')
			    *x++ = *y++;

			  /* Copy the value given, if any.  */
			  while (*y && *y != ' ' && *y != '\t')
			    *x++ = *y++;
			}
		      else
			{
			  /* Do not copy this macro - we have just done it before */
			  while (*y && *y != ' ' && *y != '\t')
			    y++;
			}
		    }
		  else if (*y == ' ' || *y == '\t')
		    /* Copy whitespace to the result.  */
		    *x++ = *y++;
		  /* Don't copy -A options  */
		  else
		    y++;
		}
	      *x++ = ' ';

	      /* Copy all of the -A options in CPP_PREDEFINES into BUF.  */
	      y = cpp_predefines;
	      while (*y != 0)
		{
		  if (! strncmp (y, "-A", 2))
		    /* Copy the whole option.  */
		    while (*y && *y != ' ' && *y != '\t')
		      *x++ = *y++;
		  else if (*y == ' ' || *y == '\t')
		    /* Copy whitespace to the result.  */
		    *x++ = *y++;
		  /* Don't copy other options.  */
		  else
		    y++;
		}

	      *x = 0;

	      value = do_spec_1 (buf, 0, NULL_PTR);
	      if (value != 0)
		return value;
	    }
	    break;

	  case 'S':
	    value = do_spec_1 (startfile_spec, 0, NULL_PTR);
	    if (value != 0)
	      return value;
	    break;

	    /* Here we define characters other than letters and digits.  */

	  case '{':
	    p = handle_braces (p);
	    if (p == 0)
	      return -1;
	    break;

	  case '%':
	    obstack_1grow (&obstack, '%');
	    break;

	  case '*':
	    do_spec_1 (soft_matched_part, 1, NULL_PTR);
	    do_spec_1 (" ", 0, NULL_PTR);
	    break;

	    /* Process a string found as the value of a spec given by name.
	       This feature allows individual machine descriptions
	       to add and use their own specs.
	       %[...] modifies -D options the way %P does;
	       %(...) uses the spec unmodified.  */
	  case '(':
	  case '[':
	    {
	      char *name = p;
	      struct spec_list *sl;
	      int len;

	      /* The string after the S/P is the name of a spec that is to be
		 processed. */
	      while (*p && *p != ')' && *p != ']')
		p++;

	      /* See if it's in the list */
	      for (len = p - name, sl = specs; sl; sl = sl->next)
		if (strncmp (sl->name, name, len) == 0 && !sl->name[len])
		  {
		    name = sl->spec;
		    break;
		  }

	      if (sl)
		{
		  if (c == '(')
		    {
		      value = do_spec_1 (name, 0, NULL_PTR);
		      if (value != 0)
			return value;
		    }
		  else
		    {
		      char *x = (char *) alloca (strlen (name) * 2 + 1);
		      char *buf = x;
		      char *y = name;

		      /* Copy all of NAME into BUF, but put __ after
			 every -D and at the end of each arg,  */
		      while (1)
			{
			  if (! strncmp (y, "-D", 2))
			    {
			      *x++ = '-';
			      *x++ = 'D';
			      *x++ = '_';
			      *x++ = '_';
			      y += 2;
			    }
			  else if (*y == ' ' || *y == 0)
			    {
			      *x++ = '_';
			      *x++ = '_';
			      if (*y == 0)
				break;
			      else
				*x++ = *y++;
			    }
			  else
			    *x++ = *y++;
			}
		      *x = 0;

		      value = do_spec_1 (buf, 0, NULL_PTR);
		      if (value != 0)
			return value;
		    }
		}

	      /* Discard the closing paren or bracket.  */
	      if (*p)
		p++;
	    }
	    break;

	  case 'v':
	    {
	      int c1 = *p++;  /* Select first or second version number.  */
	      char *v = compiler_version;
	      char *q, *copy;
	      /* If desired, advance to second version number.  */
	      if (c1 == '2')
		{
		  /* Set P after the first period.  */
		  while (*v != 0 && *v != ' ' && *v != '.')
		    v++;
		  if (*v == '.')
		    v++;
		}
	      /* Set Q at the next period or at the end.  */
	      q = v;
	      while (*q != 0 && *q != ' ' && *q != '.')
		q++;
	      /* Empty string means zero.  */
	      if (p == q)
		{
		  v = "0";
		  q = v + 1;
		}
	      /* Put that part into the command.  */
	      obstack_grow (&obstack, v, q - v);
	      arg_going = 1;
	    }
	    break;

	  case '|':
	    if (input_from_pipe)
	      do_spec_1 ("-", 0, NULL_PTR);
	    break;

	  default:
	    abort ();
	  }
	break;

      case '\\':
	/* Backslash: treat next character as ordinary.  */
	c = *p++;

	/* fall through */
      default:
	/* Ordinary character: put it into the current argument.  */
	obstack_1grow (&obstack, c);
	arg_going = 1;
      }

  return 0;		/* End of string */
}

/* Return 0 if we call do_spec_1 and that returns -1.  */

static char *
handle_braces (p)
     register char *p;
{
  register char *q;
  char *filter;
  int pipe = 0;
  int negate = 0;
  int suffix = 0;

  if (*p == '|')
    /* A `|' after the open-brace means,
       if the test fails, output a single minus sign rather than nothing.
       This is used in %{|!pipe:...}.  */
    pipe = 1, ++p;

  if (*p == '!')
    /* A `!' after the open-brace negates the condition:
       succeed if the specified switch is not present.  */
    negate = 1, ++p;

  if (*p == '.')
    /* A `.' after the open-brace means test against the current suffix.  */
    {
      if (pipe)
	abort ();

      suffix = 1;
      ++p;
    }

  filter = p;
  while (*p != ':' && *p != '}') p++;
  if (*p != '}')
    {
      register int count = 1;
      q = p + 1;
      while (count > 0)
	{
	  if (*q == '{')
	    count++;
	  else if (*q == '}')
	    count--;
	  else if (*q == 0)
	    abort ();
	  q++;
	}
    }
  else
    q = p + 1;

  if (suffix)
    {
      int found = (input_suffix != 0
		   && strlen (input_suffix) == p - filter
		   && strncmp (input_suffix, filter, p - filter) == 0);

      if (p[0] == '}')
	abort ();

      if (negate != found
	  && do_spec_1 (save_string (p + 1, q - p - 2), 0, NULL_PTR) < 0)
	return 0;

      return q;
    }
  else if (p[-1] == '*' && p[0] == '}')
    {
      /* Substitute all matching switches as separate args.  */
      register int i;
      --p;
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, filter, p - filter)
	    && check_live_switch (i, p - filter))
	  give_switch (i, 0);
    }
  else
    {
      /* Test for presence of the specified switch.  */
      register int i;
      int present = 0;

      /* If name specified ends in *, as in {x*:...},
	 check for %* and handle that case.  */
      if (p[-1] == '*' && !negate)
	{
	  int substitution;
	  char *r = p;

	  /* First see whether we have %*.  */
	  substitution = 0;
	  while (r < q)
	    {
	      if (*r == '%' && r[1] == '*')
		substitution = 1;
	      r++;
	    }
	  /* If we do, handle that case.  */
	  if (substitution)
	    {
	      /* Substitute all matching switches as separate args.
		 But do this by substituting for %*
		 in the text that follows the colon.  */

	      unsigned hard_match_len = p - filter - 1;
	      char *string = save_string (p + 1, q - p - 2);

	      for (i = 0; i < n_switches; i++)
		if (!strncmp (switches[i].part1, filter, hard_match_len)
		    && check_live_switch (i, -1))
		  {
		    do_spec_1 (string, 0, &switches[i].part1[hard_match_len]);
		    /* Pass any arguments this switch has.  */
		    give_switch (i, 1);
		  }

	      return q;
	    }
	}

      /* If name specified ends in *, as in {x*:...},
	 check for presence of any switch name starting with x.  */
      if (p[-1] == '*')
	{
	  for (i = 0; i < n_switches; i++)
	    {
	      unsigned hard_match_len = p - filter - 1;

	      if (!strncmp (switches[i].part1, filter, hard_match_len)
		  && check_live_switch (i, hard_match_len))
		{
		  present = 1;
		}
	    }
	}
      /* Otherwise, check for presence of exact name specified.  */
      else
	{
	  for (i = 0; i < n_switches; i++)
	    {
	      if (!strncmp (switches[i].part1, filter, p - filter)
		  && switches[i].part1[p - filter] == 0
		  && check_live_switch (i, -1))
		{
		  present = 1;
		  break;
		}
	    }
	}

      /* If it is as desired (present for %{s...}, absent for %{-s...})
	 then substitute either the switch or the specified
	 conditional text.  */
      if (present != negate)
	{
	  if (*p == '}')
	    {
	      give_switch (i, 0);
	    }
	  else
	    {
	      if (do_spec_1 (save_string (p + 1, q - p - 2), 0, NULL_PTR) < 0)
		return 0;
	    }
	}
      else if (pipe)
	{
	  /* Here if a %{|...} conditional fails: output a minus sign,
	     which means "standard output" or "standard input".  */
	  do_spec_1 ("-", 0, NULL_PTR);
	}
    }

  return q;
}

/* Return 0 iff switch number SWITCHNUM is obsoleted by a later switch
   on the command line.  PREFIX_LENGTH is the length of XXX in an {XXX*}
   spec, or -1 if either exact match or %* is used.

   A -O switch is obsoleted by a later -O switch.  A -f, -m, or -W switch
   whose value does not begin with "no-" is obsoleted by the same value
   with the "no-", similarly for a switch with the "no-" prefix.  */

static int
check_live_switch (switchnum, prefix_length)
     int switchnum;
     int prefix_length;
{
  char *name = switches[switchnum].part1;
  int i;

  /* In the common case of {<at-most-one-letter>*}, a negating
     switch would always match, so ignore that case.  We will just
     send the conflicting switches to the compiler phase.  */
  if (prefix_length >= 0 && prefix_length <= 1)
    return 1;

  /* If we already processed this switch and determined if it was
     live or not, return our past determination.  */
  if (switches[switchnum].live_cond != 0)
    return switches[switchnum].live_cond > 0;

  /* Now search for duplicate in a manner that depends on the name.  */
  switch (*name)
    {
    case 'O':
	for (i = switchnum + 1; i < n_switches; i++)
	  if (switches[i].part1[0] == 'O')
	    {
	      switches[switchnum].valid = 1;
	      switches[switchnum].live_cond = -1;
	      return 0;
	    }
      break;

    case 'W':  case 'f':  case 'm':
      if (! strncmp (name + 1, "no-", 3))
	{
	  /* We have Xno-YYY, search for XYYY. */
	  for (i = switchnum + 1; i < n_switches; i++)
	    if (switches[i].part1[0] == name[0]
		&& ! strcmp (&switches[i].part1[1], &name[4]))
	    {
	      switches[switchnum].valid = 1;
	      switches[switchnum].live_cond = -1;
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
	      switches[switchnum].valid = 1;
	      switches[switchnum].live_cond = -1;
	      return 0;
	    }
	}
      break;
    }

  /* Otherwise the switch is live.  */
  switches[switchnum].live_cond = 1;
  return 1;
}

/* Pass a switch to the current accumulating command
   in the same form that we received it.
   SWITCHNUM identifies the switch; it is an index into
   the vector of switches gcc received, which is `switches'.
   This cannot fail since it never finishes a command line.

   If OMIT_FIRST_WORD is nonzero, then we omit .part1 of the argument.  */

static void
give_switch (switchnum, omit_first_word)
     int switchnum;
     int omit_first_word;
{
  if (!omit_first_word)
    {
      do_spec_1 ("-", 0, NULL_PTR);
      do_spec_1 (switches[switchnum].part1, 1, NULL_PTR);
    }
  do_spec_1 (" ", 0, NULL_PTR);
  if (switches[switchnum].args != 0)
    {
      char **p;
      for (p = switches[switchnum].args; *p; p++)
	{
	  do_spec_1 (*p, 1, NULL_PTR);
	  do_spec_1 (" ", 0, NULL_PTR);
	}
    }
  switches[switchnum].valid = 1;
}

/* Search for a file named NAME trying various prefixes including the
   user's -B prefix and some standard ones.
   Return the absolute file name found.  If nothing is found, return NAME.  */

static char *
find_file (name)
     char *name;
{
  char *newname;

  /* Try multilib_dir if it is defined.  */
  if (multilib_dir != NULL)
    {
      char *try;

      try = (char *) alloca (strlen (multilib_dir) + strlen (name) + 2);
      strcpy (try, multilib_dir);
      strcat (try, dir_separator_str);
      strcat (try, name);

      newname = find_a_file (&startfile_prefixes, try, R_OK);

      /* If we don't find it in the multi library dir, then fall
	 through and look for it in the normal places.  */
      if (newname != NULL)
	return newname;
    }

  newname = find_a_file (&startfile_prefixes, name, R_OK);
  return newname ? newname : name;
}

/* Determine whether a directory exists.  If LINKER, return 0 for
   certain fixed names not needed by the linker.  If not LINKER, it is
   only important to return 0 if the host machine has a small ARG_MAX
   limit.  */

static int
is_directory (path1, path2, linker)
     char *path1;
     char *path2;
     int linker;
{
  int len1 = strlen (path1);
  int len2 = strlen (path2);
  char *path = (char *) alloca (3 + len1 + len2);
  char *cp;
  struct stat st;

#ifndef SMALL_ARG_MAX
  if (! linker)
    return 1;
#endif

  /* Construct the path from the two parts.  Ensure the string ends with "/.".
     The resulting path will be a directory even if the given path is a
     symbolic link.  */
  bcopy (path1, path, len1);
  bcopy (path2, path + len1, len2);
  cp = path + len1 + len2;
  if (cp[-1] != '/' && cp[-1] != DIR_SEPARATOR)
    *cp++ = DIR_SEPARATOR;
  *cp++ = '.';
  *cp = '\0';

  /* Exclude directories that the linker is known to search.  */
  if (linker
      && ((cp - path == 6
	   && strcmp (path, concat4 (dir_separator_str, "lib", 
                                     dir_separator_str, ".")) == 0)
	  || (cp - path == 10
	      && strcmp (path, concat6 (dir_separator_str, "usr", 
                                        dir_separator_str, "lib", 
                                        dir_separator_str, ".")) == 0)))
    return 0;

  return (stat (path, &st) >= 0 && S_ISDIR (st.st_mode));
}

/* On fatal signals, delete all the temporary files.  */

static void
fatal_error (signum)
     int signum;
{
  signal (signum, SIG_DFL);
  delete_failure_queue ();
  delete_temp_files ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  register int i;
  int j;
  int value;
  int linker_was_run = 0;
  char *explicit_link_files;
  char *specs_file;
  char *p;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/' && p[-1] != DIR_SEPARATOR) --p;
  programname = p;

  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, fatal_error);
#ifdef SIGHUP
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, fatal_error);
#endif
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, fatal_error);
#ifdef SIGPIPE
  if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
    signal (SIGPIPE, fatal_error);
#endif

  argbuf_length = 10;
  argbuf = (char **) xmalloc (argbuf_length * sizeof (char *));

  obstack_init (&obstack);

  /* Set up to remember the pathname of gcc and any options
     needed for collect.  We use argv[0] instead of programname because
     we need the complete pathname.  */
  obstack_init (&collect_obstack);
  obstack_grow (&collect_obstack, "COLLECT_GCC=", sizeof ("COLLECT_GCC=")-1);
  obstack_grow (&collect_obstack, argv[0], strlen (argv[0])+1);
  putenv (obstack_finish (&collect_obstack));

#ifdef INIT_ENVIRONMENT
  /* Set up any other necessary machine specific environment variables.  */
  putenv (INIT_ENVIRONMENT);
#endif

  /* Choose directory for temp files.  */

  choose_temp_base ();

  /* Make a table of what switches there are (switches, n_switches).
     Make a table of specified input files (infiles, n_infiles).
     Decode switches that are handled locally.  */

  process_command (argc, argv);

  /* Initialize the vector of specs to just the default.
     This means one element containing 0s, as a terminator.  */

  compilers = (struct compiler *) xmalloc (sizeof default_compilers);
  bcopy ((char *) default_compilers, (char *) compilers,
	 sizeof default_compilers);
  n_compilers = n_default_compilers;

  /* Read specs from a file if there is one.  */

  machine_suffix = concat4 (spec_machine, dir_separator_str,
                            spec_version, dir_separator_str);
  just_machine_suffix = concat (spec_machine, dir_separator_str);

  specs_file = find_a_file (&startfile_prefixes, "specs", R_OK);
  /* Read the specs file unless it is a default one.  */
  if (specs_file != 0 && strcmp (specs_file, "specs"))
    read_specs (specs_file);

  /* If not cross-compiling, look for startfiles in the standard places.  */
  /* The fact that these are done here, after reading the specs file,
     means that it cannot be found in these directories.
     But that's okay.  It should never be there anyway.  */
  if (!cross_compile)
    {
#ifdef MD_EXEC_PREFIX
      add_prefix (&exec_prefixes, md_exec_prefix, 0, 0, NULL_PTR);
      add_prefix (&startfile_prefixes, md_exec_prefix, 0, 0, NULL_PTR);
#endif

#ifdef MD_STARTFILE_PREFIX
      add_prefix (&startfile_prefixes, md_startfile_prefix, 0, 0, NULL_PTR);
#endif

#ifdef MD_STARTFILE_PREFIX_1
      add_prefix (&startfile_prefixes, md_startfile_prefix_1, 0, 0, NULL_PTR);
#endif

      /* If standard_startfile_prefix is relative, base it on
	 standard_exec_prefix.  This lets us move the installed tree
	 as a unit.  If GCC_EXEC_PREFIX is defined, base
	 standard_startfile_prefix on that as well.  */
      if (*standard_startfile_prefix == '/'
	  || *standard_startfile_prefix == DIR_SEPARATOR)
	add_prefix (&startfile_prefixes, standard_startfile_prefix, 0, 0,
		    NULL_PTR);
      else
	{
	  if (gcc_exec_prefix)
	    add_prefix (&startfile_prefixes,
			concat3 (gcc_exec_prefix, machine_suffix,
				 standard_startfile_prefix),
			0, 0, NULL_PTR);
	  add_prefix (&startfile_prefixes,
		      concat3 (standard_exec_prefix,
			       machine_suffix,
			       standard_startfile_prefix),
		      0, 0, NULL_PTR);
	}		       

      add_prefix (&startfile_prefixes, standard_startfile_prefix_1, 0, 0,
		  NULL_PTR);
      add_prefix (&startfile_prefixes, standard_startfile_prefix_2, 0, 0,
		  NULL_PTR);
#if 0 /* Can cause surprises, and one can use -B./ instead.  */
      add_prefix (&startfile_prefixes, "./", 0, 1, NULL_PTR);
#endif
    }
  else
    {
      if (*standard_startfile_prefix != DIR_SEPARATOR && gcc_exec_prefix)
	add_prefix (&startfile_prefixes,
		    concat3 (gcc_exec_prefix, machine_suffix,
			     standard_startfile_prefix),
		    0, 0, NULL_PTR);
    }

  /* If we have a GCC_EXEC_PREFIX envvar, modify it for cpp's sake.  */
  if (gcc_exec_prefix)
    {
      char * temp = (char *) xmalloc (strlen (gcc_exec_prefix)
				      + strlen (spec_version)
				      + strlen (spec_machine) + 3);
      strcpy (temp, gcc_exec_prefix);
      strcat (temp, spec_machine);
      strcat (temp, dir_separator_str);
      strcat (temp, spec_version);
      strcat (temp, dir_separator_str);
      gcc_exec_prefix = temp;
    }

  /* Now we have the specs.
     Set the `valid' bits for switches that match anything in any spec.  */

  validate_all_switches ();

  /* Now that we have the switches and the specs, set
     the subdirectory based on the options.  */
  set_multilib_dir ();

  /* Warn about any switches that no pass was interested in.  */

  for (i = 0; i < n_switches; i++)
    if (! switches[i].valid)
      error ("unrecognized option `-%s'", switches[i].part1);

  /* Obey some of the options.  */

  if (print_search_dirs)
    {
      printf ("install: %s%s\n", standard_exec_prefix, machine_suffix);
      printf ("programs: %s\n", build_search_list (&exec_prefixes, "", 0));
      printf ("libraries: %s\n", build_search_list (&startfile_prefixes, "", 0));
      exit (0);
    }

  if (print_file_name)
    {
      printf ("%s\n", find_file (print_file_name));
      exit (0);
    }

  if (print_prog_name)
    {
      char *newname = find_a_file (&exec_prefixes, print_prog_name, X_OK);
      printf ("%s\n", (newname ? newname : print_prog_name));
      exit (0);
    }

  if (print_multi_lib)
    {
      print_multilib_info ();
      exit (0);
    }

  if (print_multi_directory)
    {
      if (multilib_dir == NULL)
	printf (".\n");
      else
	printf ("%s\n", multilib_dir);
      exit (0);
    }

  if (verbose_flag)
    {
      if (! strcmp (version_string, compiler_version))
	fprintf (stderr, "gcc version %s\n", version_string);
      else
	fprintf (stderr, "gcc driver version %s executing gcc version %s\n",
		 version_string, compiler_version);

      if (n_infiles == 0)
	exit (0);
    }

  if (n_infiles == 0)
    fatal ("No input files");

  /* Make a place to record the compiler output file names
     that correspond to the input files.  */

  outfiles = (char **) xmalloc (n_infiles * sizeof (char *));
  bzero ((char *) outfiles, n_infiles * sizeof (char *));

  /* Record which files were specified explicitly as link input.  */

  explicit_link_files = xmalloc (n_infiles);
  bzero (explicit_link_files, n_infiles);

  for (i = 0; i < n_infiles; i++)
    {
      register struct compiler *cp = 0;
      int this_file_error = 0;

      /* Tell do_spec what to substitute for %i.  */

      input_filename = infiles[i].name;
      input_filename_length = strlen (input_filename);
      input_file_number = i;

      /* Use the same thing in %o, unless cp->spec says otherwise.  */

      outfiles[i] = input_filename;

      /* Figure out which compiler from the file's suffix.  */

      cp = lookup_compiler (infiles[i].name, input_filename_length,
			    infiles[i].language);

      if (cp)
	{
	  /* Ok, we found an applicable compiler.  Run its spec.  */
	  /* First say how much of input_filename to substitute for %b  */
	  register char *p;
	  int len;

	  input_basename = input_filename;
	  for (p = input_filename; *p; p++)
	    if (*p == '/' || *p == DIR_SEPARATOR)
	      input_basename = p + 1;

	  /* Find a suffix starting with the last period,
	     and set basename_length to exclude that suffix.  */
	  basename_length = strlen (input_basename);
	  p = input_basename + basename_length;
	  while (p != input_basename && *p != '.') --p;
	  if (*p == '.' && p != input_basename)
	    {
	      basename_length = p - input_basename;
	      input_suffix = p + 1;
	    }
	  else
	    input_suffix = "";

	  len = 0;
	  for (j = 0; j < sizeof cp->spec / sizeof cp->spec[0]; j++)
	    if (cp->spec[j])
	      len += strlen (cp->spec[j]);

	  p = (char *) xmalloc (len + 1);

	  len = 0;
	  for (j = 0; j < sizeof cp->spec / sizeof cp->spec[0]; j++)
	    if (cp->spec[j])
	      {
		strcpy (p + len, cp->spec[j]);
		len += strlen (cp->spec[j]);
	      }

	  value = do_spec (p);
	  free (p);
	  if (value < 0)
	    this_file_error = 1;
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
	  error_count++;
	}
      /* If this compilation succeeded, don't delete those files later.  */
      clear_failure_queue ();
    }

  /* Run ld to link all the compiler output files.  */

  if (error_count == 0)
    {
      int tmp = execution_count;
      int i;
      int first_time;

      /* Rebuild the COMPILER_PATH and LIBRARY_PATH environment variables
	 for collect.  */
      putenv_from_prefixes (&exec_prefixes, "COMPILER_PATH=");
      putenv_from_prefixes (&startfile_prefixes, "LIBRARY_PATH=");

      /* Build COLLECT_GCC_OPTIONS to have all of the options specified to
	 the compiler.  */
      obstack_grow (&collect_obstack, "COLLECT_GCC_OPTIONS=",
		    sizeof ("COLLECT_GCC_OPTIONS=")-1);

      first_time = TRUE;
      for (i = 0; i < n_switches; i++)
	{
	  char **args;
	  if (!first_time)
	    obstack_grow (&collect_obstack, " ", 1);

	  first_time = FALSE;
	  obstack_grow (&collect_obstack, "-", 1);
	  obstack_grow (&collect_obstack, switches[i].part1,
			strlen (switches[i].part1));

	  for (args = switches[i].args; args && *args; args++)
	    {
	      obstack_grow (&collect_obstack, " ", 1);
	      obstack_grow (&collect_obstack, *args, strlen (*args));
	    }
	}
      obstack_grow (&collect_obstack, "\0", 1);
      putenv (obstack_finish (&collect_obstack));

      value = do_spec (link_command_spec);
      if (value < 0)
	error_count = 1;
      linker_was_run = (tmp != execution_count);
    }

  /* Warn if a -B option was specified but the prefix was never used.  */
  unused_prefix_warnings (&exec_prefixes);
  unused_prefix_warnings (&startfile_prefixes);

  /* If options said don't run linker,
     complain about input files to be given to the linker.  */

  if (! linker_was_run && error_count == 0)
    for (i = 0; i < n_infiles; i++)
      if (explicit_link_files[i])
	error ("%s: linker input file unused since linking not done",
	       outfiles[i]);

  /* Delete some or all of the temporary files we made.  */

  if (error_count)
    delete_failure_queue ();
  delete_temp_files ();

  exit (error_count > 0 ? (signal_count ? 2 : 1) : 0);
  /* NOTREACHED */
  return 0;
}

/* Find the proper compilation spec for the file name NAME,
   whose length is LENGTH.  LANGUAGE is the specified language,
   or 0 if none specified.  */

static struct compiler *
lookup_compiler (name, length, language)
     char *name;
     int length;
     char *language;
{
  struct compiler *cp;

  /* Look for the language, if one is spec'd.  */
  if (language != 0)
    {
      for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
	{
	  if (language != 0)
	    {
	      if (cp->suffix[0] == '@'
		  && !strcmp (cp->suffix + 1, language))
		return cp;
	    }
	}
      error ("language %s not recognized", language);
    }

  /* Look for a suffix.  */
  for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
    {
      if (/* The suffix `-' matches only the file name `-'.  */
	  (!strcmp (cp->suffix, "-") && !strcmp (name, "-"))
	  ||
	  (strlen (cp->suffix) < length
	   /* See if the suffix matches the end of NAME.  */
#ifdef OS2
	   && (!strcmp (cp->suffix,
			name + length - strlen (cp->suffix))
	    || !strpbrk (cp->suffix, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
	     && !strcasecmp (cp->suffix,
			  name + length - strlen (cp->suffix)))))
#else
	   && !strcmp (cp->suffix,
		       name + length - strlen (cp->suffix))))
#endif
	{
	  if (cp->spec[0][0] == '@')
	    {
	      struct compiler *new;
	      /* An alias entry maps a suffix to a language.
		 Search for the language; pass 0 for NAME and LENGTH
		 to avoid infinite recursion if language not found.
		 Construct the new compiler spec.  */
	      language = cp->spec[0] + 1;
	      new = (struct compiler *) xmalloc (sizeof (struct compiler));
	      new->suffix = cp->suffix;
	      bcopy ((char *) lookup_compiler (NULL_PTR, 0, language)->spec,
		     (char *) new->spec, sizeof new->spec);
	      return new;
	    }
	  /* A non-alias entry: return it.  */
	  return cp;
	}
    }

  return 0;
}

char *
xmalloc (size)
     unsigned size;
{
  register char *value = (char *) malloc (size);
  if (value == 0)
    fatal ("virtual memory exhausted");
  return value;
}

char *
xrealloc (ptr, size)
     char *ptr;
     unsigned size;
{
  register char *value = (char *) realloc (ptr, size);
  if (value == 0)
    fatal ("virtual memory exhausted");
  return value;
}

/* Return a newly-allocated string whose contents concatenate those of s1, s2 */

static char *
concat (s1, s2)
     char *s1, *s2;
{
  int len1 = strlen (s1);
  int len2 = strlen (s2);
  char *result = xmalloc (len1 + len2 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  *(result + len1 + len2) = 0;

  return result;
}

static char *
concat3 (s1, s2, s3)
     char *s1, *s2, *s3;
{
  return concat (concat (s1, s2), s3);
}

static char *
concat4 (s1, s2, s3, s4)
     char *s1, *s2, *s3, *s4;
{
  return concat (concat (s1, s2), concat (s3, s4));
}

static char *
concat6 (s1, s2, s3, s4, s5, s6)
     char *s1, *s2, *s3, *s4, *s5, *s6;
{
  return concat3 (concat (s1, s2), concat (s3, s4), concat (s5, s6));
}

static char *
save_string (s, len)
     char *s;
     int len;
{
  register char *result = xmalloc (len + 1);

  bcopy (s, result, len);
  result[len] = 0;
  return result;
}

static void
pfatal_with_name (name)
     char *name;
{
  char *s;

  if (errno < sys_nerr)
    s = concat ("%s: ", my_strerror( errno ));
  else
    s = "cannot open `%s'";
  fatal (s, name);
}

static void
perror_with_name (name)
     char *name;
{
  char *s;

  if (errno < sys_nerr)
    s = concat ("%s: ", my_strerror( errno ));
  else
    s = "cannot open `%s'";
  error (s, name);
}

static void
perror_exec (name)
     char *name;
{
  char *s;

  if (errno < sys_nerr)
    s = concat ("installation problem, cannot exec `%s': ",
		my_strerror (errno));
  else
    s = "installation problem, cannot exec `%s'";
  error (s, name);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

#ifdef HAVE_VPRINTF

/* Output an error message and exit */

static void
fatal VPROTO((char *format, ...))
{
#ifndef __STDC__
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef __STDC__
  format = va_arg (ap, char*);
#endif

  fprintf (stderr, "%s: ", programname);
  vfprintf (stderr, format, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  delete_temp_files ();
  exit (1);
}

static void
error VPROTO((char *format, ...))
{
#ifndef __STDC__
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef __STDC__
  format = va_arg (ap, char*);
#endif

  fprintf (stderr, "%s: ", programname);
  vfprintf (stderr, format, ap);
  va_end (ap);

  fprintf (stderr, "\n");
}

#else /* not HAVE_VPRINTF */

static void
fatal (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  error (msg, arg1, arg2);
  delete_temp_files ();
  exit (1);
}

static void
error (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  fprintf (stderr, "%s: ", programname);
  fprintf (stderr, msg, arg1, arg2);
  fprintf (stderr, "\n");
}

#endif /* not HAVE_VPRINTF */


static void
validate_all_switches ()
{
  struct compiler *comp;
  register char *p;
  register char c;
  struct spec_list *spec;

  for (comp = compilers; comp->spec[0]; comp++)
    {
      int i;
      for (i = 0; i < sizeof comp->spec / sizeof comp->spec[0] && comp->spec[i]; i++)
	{
	  p = comp->spec[i];
	  while (c = *p++)
	    if (c == '%' && *p == '{')
	      /* We have a switch spec.  */
	      validate_switches (p + 1);
	}
    }

  /* look through the linked list of extra specs read from the specs file */
  for (spec = specs; spec ; spec = spec->next)
    {
      p = spec->spec;
      while (c = *p++)
	if (c == '%' && *p == '{')
	  /* We have a switch spec.  */
	  validate_switches (p + 1);
    }

  p = link_command_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  /* Now notice switches mentioned in the machine-specific specs.  */

  p = asm_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = asm_final_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = cpp_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = signed_char_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = cc1_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = cc1plus_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = link_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = lib_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = libgcc_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);

  p = startfile_spec;
  while (c = *p++)
    if (c == '%' && *p == '{')
      /* We have a switch spec.  */
      validate_switches (p + 1);
}

/* Look at the switch-name that comes after START
   and mark as valid all supplied switches that match it.  */

static void
validate_switches (start)
     char *start;
{
  register char *p = start;
  char *filter;
  register int i;
  int suffix = 0;

  if (*p == '|')
    ++p;

  if (*p == '!')
    ++p;

  if (*p == '.')
    suffix = 1, ++p;

  filter = p;
  while (*p != ':' && *p != '}') p++;

  if (suffix)
    ;
  else if (p[-1] == '*')
    {
      /* Mark all matching switches as valid.  */
      --p;
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, filter, p - filter))
	  switches[i].valid = 1;
    }
  else
    {
      /* Mark an exact matching switch as valid.  */
      for (i = 0; i < n_switches; i++)
	{
	  if (!strncmp (switches[i].part1, filter, p - filter)
	      && switches[i].part1[p - filter] == 0)
	    switches[i].valid = 1;
	}
    }
}

/* Check whether a particular argument was used.  */

static int
used_arg (p, len)
     char *p;
     int len;
{
  int i;

  for (i = 0; i < n_switches; i++)
    if (! strncmp (switches[i].part1, p, len)
	&& strlen (switches[i].part1) == len)
      return 1;
  return 0;
}

/* Check whether a particular argument is a default argument.  */

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { NULL }
#endif

static char *multilib_defaults[] = MULTILIB_DEFAULTS;

static int
default_arg (p, len)
     char *p;
     int len;
{
  int count = sizeof multilib_defaults / sizeof multilib_defaults[0];
  int i;

  for (i = 0; i < count; i++)
    if (multilib_defaults[i] != NULL
	&& strncmp (multilib_defaults[i], p, len) == 0
	&& multilib_defaults[i][len] == '\0')
      return 1;

  return 0;
}

/* Work out the subdirectory to use based on the
   options.  The format of multilib_select is a list of elements.
   Each element is a subdirectory name followed by a list of options
   followed by a semicolon.  gcc will consider each line in turn.  If
   none of the options beginning with an exclamation point are
   present, and all of the other options are present, that
   subdirectory will be used.  */

static void
set_multilib_dir ()
{
  char *p = multilib_select;
  int this_path_len;
  char *this_path, *this_arg;
  int not_arg;
  int ok;

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
	    abort ();
	  ++p;
	}
      this_path_len = p - this_path;

      /* Check the arguments.  */
      ok = 1;
      ++p;
      while (*p != ';')
	{
	  if (*p == '\0')
	    abort ();

	  if (! ok)
	    {
	      ++p;
	      continue;
	    }

	  this_arg = p;
	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		abort ();
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
	  if (! default_arg (this_arg, p - this_arg))
	    {
	      ok = used_arg (this_arg, p - this_arg);
	      if (not_arg)
		ok = ! ok;
	    }

	  if (*p == ' ')
	    ++p;
	}

      if (ok)
	{
	  if (this_path_len != 1
	      || this_path[0] != '.')
	    {
	      multilib_dir = xmalloc (this_path_len + 1);
	      strncpy (multilib_dir, this_path, this_path_len);
	      multilib_dir[this_path_len] = '\0';
	    }
	  break;
	}

      ++p;
    }      
}

/* Print out the multiple library subdirectory selection
   information.  This prints out a series of lines.  Each line looks
   like SUBDIRECTORY;@OPTION@OPTION, with as many options as is
   required.  Only the desired options are printed out, the negative
   matches.  The options are print without a leading dash.  There are
   no spaces to make it easy to use the information in the shell.
   Each subdirectory is printed only once.  This assumes the ordering
   generated by the genmultilib script.  */

static void
print_multilib_info ()
{
  char *p = multilib_select;
  char *last_path = 0, *this_path;
  int skip;
  int last_path_len = 0;

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
	    abort ();
	  ++p;
	}

      /* If this is a duplicate, skip it.  */
      skip = (last_path != 0 && p - this_path == last_path_len
	      && ! strncmp (last_path, this_path, last_path_len));

      last_path = this_path;
      last_path_len = p - this_path;

      /* If this directory requires any default arguments, we can skip
	 it.  We will already have printed a directory identical to
	 this one which does not require that default argument.  */
      if (! skip)
	{
	  char *q;

	  q = p + 1;
	  while (*q != ';')
	    {
	      char *arg;

	      if (*q == '\0')
		abort ();

	      if (*q == '!')
		arg = NULL;
	      else
		arg = q;

	      while (*q != ' ' && *q != ';')
		{
		  if (*q == '\0')
		    abort ();
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
	  char *p1;

	  for (p1 = last_path; p1 < p; p1++)
	    putchar (*p1);
	  putchar (';');
	}

      ++p;
      while (*p != ';')
	{
	  int use_arg;

	  if (*p == '\0')
	    abort ();

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
		abort ();
	      if (use_arg)
		putchar (*p);
	      ++p;
	    }

	  if (*p == ' ')
	    ++p;
	}

      if (! skip)
	putchar ('\n');

      ++p;
    }
}
