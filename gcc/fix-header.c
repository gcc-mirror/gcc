/* fix-header.c - Make C header file suitable for C++.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* This program massages a system include file (such as stdio.h),
   into a form more conforming with ANSI/POSIX, and more suitable for C++:

   * extern "C" { ... } braces are added (inside #ifndef __cplusplus),
   if they seem to be needed.  These prevent C++ compilers from name
   mangling the functions inside the braces.

   * If an old-style incomplete function declaration is seen (without
   an argument list), and it is a "standard" function listed in
   the file sys-protos.h (and with a non-empty argument list), then
   the declaration is converted to a complete prototype by replacing
   the empty parameter list with the argument lust from sys-protos.h.

   * The program can be given a list of (names of) required standard
   functions (such as fclose for stdio.h).  If a required function
   is not seen in the input, then a prototype for it will be
   written to the output.

   * If all of the non-comment code of the original file is protected
   against multiple inclusion:
	#ifndef FOO
	#define FOO
	<body of include file>
	#endif
   then extra matter added to the include file is placed inside the <body>.

   * If the input file is OK (nothing needs to be done);
   the output file is not written (nor removed if it exists).

   There are also some special actions that are done for certain
   well-known standard include files:

   * If argv[1] is "sys/stat.h", the Posix.1 macros
   S_ISBLK, S_ISCHR, S_ISDIR, S_ISFIFO, S_ISLNK, S_ISREG are added if
   they were missing, and the corresponding "traditional" S_IFxxx
   macros were defined.

   * If argv[1] is "errno.h", errno is declared if it was missing.

   * TODO:  The input file should be read complete into memory, because:
   a) it needs to be scanned twice anyway, and
   b) it would be nice to allow update in place.

   Usage:
	fix-header FOO.H INFILE.H OUTFILE.H [OPTIONS]
   where:
   * FOO.H is the relative file name of the include file,
   as it would be #include'd by a C file.  (E.g. stdio.h)
   * INFILE.H is a full pathname for the input file (e.g. /usr/include/stdio.h)
   * OUTFILE.H is the full pathname for where to write the output file,
   if anything needs to be done.  (e.g. ./include/stdio.h)
   * OPTIONS are such as you would pass to cpp.

   Written by Per Bothner <bothner@cygnus.com>, July 1993. */

#include <stdio.h>
#include <ctype.h>
#include "hconfig.h"
#include "obstack.h"
#include "scan.h"
#include "cpplib.h"
#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#if !__STDC__
#define const /* nothing */
#endif

sstring buf;

int verbose = 0;
int partial_count = 0;
int warnings = 0;

/* We no longer need to add extern "C", because cpp implicitly
   forces the standard include files to be treated as C.  */
/*#define ADD_MISSING_EXTERN_C 1 */

#if ADD_MISSING_EXTERN_C
int missing_extern_C_count = 0;
#endif
int missing_errno = 0;

#include "xsys-protos.h"

#ifdef FIXPROTO_IGNORE_LIST
/* This is a currently unused feature. */

/* List of files and directories to ignore.
   A directory name (ending in '/') means ignore anything in that
   directory.  (It might be more efficient to do directory pruning
   earlier in fixproto, but this is simpler and easier to customize.) */

static char *files_to_ignore[] = {
  "X11/",
  FIXPROTO_IGNORE_LIST
  0
};
#endif

char *inf_buffer;
char *inf_limit;
char *inf_ptr;

/* Certain standard files get extra treatment */

enum special_file
{
  no_special,
  errno_h,
  stdio_h,
  sys_stat_h
};

/* A NAMELIST is a sequence of names, separated by '\0', and terminated
   by an empty name (i.e. by "\0\0"). */

typedef const char* namelist;

struct std_include_entry {
  const char *name;
  namelist required;
  namelist extra;
  int special;
};

/* End of namelist NAMES. */

namelist
namelist_end (names)
     namelist names;
{
  register namelist ptr;
  for (ptr = names; ; ptr++)
    {
      if (*ptr == '\0')
	{
	  ptr++;
	  if (*ptr == '\0')
	    return ptr;
	}
    }
}

const char NONE[] = "";

struct std_include_entry *include_entry;

struct std_include_entry std_include_table [] = {
  { "ctype.h",
      "isalnum\0isalpha\0iscntrl\0isdigit\0isgraph\0islower\0\
isprint\0ispunct\0isspace\0isupper\0isxdigit\0tolower\0toupper\0", NONE },

  { "dirent.h", "closedir\0opendir\0readdir\0rewinddir\0", NONE},

  { "errno.h", NONE, "errno\0" },

  { "curses.h", "box\0delwin\0endwin\0getcurx\0getcury\0initscr\0\
mvcur\0mvwprintw\0mvwscanw\0newwin\0overlay\0overwrite\0\
scroll\0subwin\0touchwin\0waddstr\0wclear\0wclrtobot\0wclrtoeol\0\
waddch\0wdelch\0wdeleteln\0werase\0wgetch\0wgetstr\0winsch\0winsertln\0\
wmove\0wprintw\0wrefresh\0wscanw\0wstandend\0wstandout\0", NONE },

  { "fcntl.h", "creat\0fcntl\0open\0", NONE },

  /* Maybe also "getgrent fgetgrent setgrent endgrent" */
  { "grp.h", "getgrgid\0getgrnam\0", NONE },

/*{ "limit.h", ... provided by gcc }, */

  { "locale.h", "localeconv\0setlocale\0", NONE },

  { "math.h", "acos\0asin\0atan\0atan2\0ceil\0cos\0cosh\0exp\0\
fabs\0floor\0fmod\0frexp\0ldexp\0log10\0log\0modf\0pow\0sin\0sinh\0sqrt\0\
tan\0tanh\0", "HUGE_VAL\0" },

  { "pwd.h", "getpwnam\0getpwuid\0", NONE },

  /* Left out siglongjmp sigsetjmp - these depend on sigjmp_buf. */
  { "setjmp.h", "longjmp\0setjmp\0", NONE },

  /* Left out signal() - its prototype is too complex for us!
     Also left out "sigaction sigaddset sigdelset sigemptyset
     sigfillset sigismember sigpending sigprocmask sigsuspend"
     because these need sigset_t or struct sigaction.
     Most systems that provide them will also declare them. */
  { "signal.h", "kill\0raise\0", NONE },

  { "stdio.h", "clearerr\0fclose\0feof\0ferror\0fflush\0fgetc\0fgetpos\0\
fgets\0fopen\0fprintf\0fputc\0fputs\0fread\0freopen\0fscanf\0fseek\0\
fsetpos\0ftell\0fwrite\0getc\0getchar\0gets\0pclose\0perror\0popen\0\
printf\0putc\0putchar\0puts\0remove\0rename\0rewind\0scanf\0setbuf\0\
setvbuf\0sprintf\0sscanf\0vprintf\0vsprintf\0vfprintf\0tmpfile\0\
tmpnam\0ungetc\0", NONE },
/* Should perhaps also handle NULL, EOF, ... ? */

  /* "div ldiv", - ignored because these depend on div_t, ldiv_t
     ignore these: "mblen mbstowcs mbstowc wcstombs wctomb"
     Left out getgroups, because SunOS4 has incompatible BSD and SVR4 versions.
     Should perhaps also add NULL */
  { "stdlib.h", "abort\0abs\0atexit\0atof\0atoi\0atol\0bsearch\0calloc\0\
exit\0free\0getenv\0labs\0malloc\0putenv\0qsort\0rand\0realloc\0\
srand\0strtod\0strtol\0strtoul\0system\0", NONE },

  { "string.h", "memchr\0memcmp\0memcpy\0memmove\0memset\0\
strcat\0strchr\0strcmp\0strcoll\0strcpy\0strcspn\0strerror\0\
strlen\0strncat\0strncmp\0strncpy\0strpbrk\0strrchr\0strspn\0strstr\0\
strtok\0strxfrm\0", NONE },
/* Should perhaps also add NULL and size_t */

  { "sys/stat.h", "chmod\0fstat\0mkdir\0mkfifo\0stat\0lstat\0umask\0",
      "S_ISDIR\0S_ISBLK\0S_ISCHR\0S_ISFIFO\0S_ISREG\0S_ISLNK\0S_IFDIR\0\
S_IFBLK\0S_IFCHR\0S_IFIFO\0S_IFREG\0S_IFLNK\0" },

  { "sys/times.h", "times\0", NONE },
  /* "sys/types.h" add types (not in old g++-include) */

  { "sys/utsname.h", "uname\0", NONE },

  { "sys/wait.h", "wait\0waitpid\0",
      "WEXITSTATUS\0WIFEXITED\0WIFSIGNALED\0WIFSTOPPED\0WSTOPSIG\0\
WTERMSIG\0WNOHANG\0WNOTRACED\0" },

  { "tar.h", NONE, NONE },

  { "termios.h", "cfgetispeed\0cfgetospeed\0cfsetispeed\0cfsetospeed\0tcdrain\0tcflow\0tcflush\0tcgetattr\0tcsendbreak\0tcsetattr\0", NONE },

  { "time.h", "asctime\0clock\0ctime\0difftime\0gmtime\0localtime\0mktime\0strftime\0time\0tzset\0", NONE },

  { "unistd.h", "_exit\0access\0alarm\0chdir\0chown\0close\0ctermid\0cuserid\0\
dup\0dup2\0execl\0execle\0execlp\0execv\0execve\0execvp\0fork\0fpathconf\0\
getcwd\0getegid\0geteuid\0getgid\0getlogin\0getopt\0getpgrp\0getpid\0\
getppid\0getuid\0isatty\0link\0lseek\0pathconf\0pause\0pipe\0read\0rmdir\0\
setgid\0setpgid\0setsid\0setuid\0sleep\0sysconf\0tcgetpgrp\0tcsetpgrp\0\
ttyname\0unlink\0write\0", NONE },

  { 0, NONE, NONE }
};

enum special_file special_file_handling = no_special;

/* The following are only used when handling sys/stat.h */
/* They are set if the corresponding macro has been seen. */
int seen_S_IFBLK = 0, seen_S_ISBLK  = 0;
int seen_S_IFCHR = 0, seen_S_ISCHR  = 0;
int seen_S_IFDIR = 0, seen_S_ISDIR  = 0;
int seen_S_IFIFO = 0, seen_S_ISFIFO = 0;
int seen_S_IFLNK = 0, seen_S_ISLNK  = 0;
int seen_S_IFREG = 0, seen_S_ISREG  = 0;

/* Wrapper around free, to avoid prototype clashes. */

void
xfree (ptr)
     char *ptr;
{
  free (ptr);
}

/* Avoid error if config defines abort as fancy_abort.
   It's not worth "really" implementing this because ordinary
   compiler users never run fix-header.  */

void
fancy_abort ()
{
  abort ();
}

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free xfree
struct obstack scan_file_obstack;

/* NOTE:  If you edit this, also edit gen-protos.c !! */
struct fn_decl *
lookup_std_proto (name, name_length)
     const char *name;
     int name_length;
{
  int i = hashf (name, name_length, HASH_SIZE);
  int i0 = i;
  for (;;)
    {
      struct fn_decl *fn;
      if (hash_tab[i] == 0)
	return NULL;
      fn = &std_protos[hash_tab[i]];
      if (strlen (fn->fname) == name_length
	  && strncmp (fn->fname, name, name_length) == 0)
	return fn;
      i = (i+1) % HASH_SIZE;
      if (i == i0)
	abort ();
    }
}

char *inc_filename;
int inc_filename_length;
char *progname = "fix-header";
FILE *outf;
sstring line;

int lbrac_line, rbrac_line;

namelist required_functions_list;
int required_unseen_count = 0;

void 
write_lbrac ()
{
  
#if ADD_MISSING_EXTERN_C
  if (missing_extern_C_count + required_unseen_count > 0)
    fprintf (outf, "#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
#endif

  if (partial_count)
    {
      fprintf (outf, "#ifndef _PARAMS\n");
      fprintf (outf, "#if defined(__STDC__) || defined(__cplusplus)\n");
      fprintf (outf, "#define _PARAMS(ARGS) ARGS\n");
      fprintf (outf, "#else\n");
      fprintf (outf, "#define _PARAMS(ARGS) ()\n");
      fprintf (outf, "#endif\n#endif /* _PARAMS */\n");
    }
}

struct partial_proto
{
  struct partial_proto *next;
  char *fname;	/* name of function */
  char *rtype;	/* return type */
  struct fn_decl *fn;
  int line_seen;
};

struct partial_proto *partial_proto_list = NULL;

struct partial_proto required_dummy_proto, seen_dummy_proto;
#define REQUIRED(FN) ((FN)->partial == &required_dummy_proto)
#define SET_REQUIRED(FN) ((FN)->partial = &required_dummy_proto)
#define SET_SEEN(FN) ((FN)->partial = &seen_dummy_proto)
#define SEEN(FN) ((FN)->partial == &seen_dummy_proto)

void
recognized_macro (fname)
     char *fname;
{
  /* The original include file defines fname as a macro. */
  struct fn_decl *fn = lookup_std_proto (fname, strlen (fname));

  /* Since fname is a macro, don't require a prototype for it. */
  if (fn)
    {
      if (REQUIRED (fn))
	required_unseen_count--;
      SET_SEEN (fn);
    }

  switch (special_file_handling)
    {
    case errno_h:
      if (strcmp (fname, "errno") == 0) missing_errno = 0;
      break;
    case sys_stat_h:
      if (fname[0] == 'S' && fname[1] == '_')
	{
	  if (strcmp (fname, "S_IFBLK") == 0) seen_S_IFBLK++;
	  else if (strcmp (fname, "S_ISBLK") == 0) seen_S_ISBLK++;
	  else if (strcmp (fname, "S_IFCHR") == 0) seen_S_IFCHR++;
	  else if (strcmp (fname, "S_ISCHR") == 0) seen_S_ISCHR++;
	  else if (strcmp (fname, "S_IFDIR") == 0) seen_S_IFDIR++;
	  else if (strcmp (fname, "S_ISDIR") == 0) seen_S_ISDIR++;
	  else if (strcmp (fname, "S_IFIFO") == 0) seen_S_IFIFO++;
	  else if (strcmp (fname, "S_ISFIFO") == 0) seen_S_ISFIFO++;
	  else if (strcmp (fname, "S_IFLNK") == 0) seen_S_IFLNK++;
	  else if (strcmp (fname, "S_ISLNK") == 0) seen_S_ISLNK++;
	  else if (strcmp (fname, "S_IFREG") == 0) seen_S_IFREG++;
	  else if (strcmp (fname, "S_ISREG") == 0) seen_S_ISREG++;
	}
    }
}

void
recognized_extern (name, name_length, type, type_length)
     char *name;
     char *type;
     int name_length, type_length;
{
  switch (special_file_handling)
    {
    case errno_h:
      if (strncmp (name, "errno", name_length) == 0) missing_errno = 0;
      break;
    }
}

/* Called by scan_decls if it saw a function definition for a function
   named FNAME, with return type RTYPE, and argument list ARGS,
   in source file FILE_SEEN on line LINE_SEEN.
   KIND is 'I' for an inline function;
   'F' if a normal function declaration preceded by 'extern "C"'
   (or nested inside 'extern "C"' braces); or
   'f' for other function declarations. */

void
recognized_function (fname, fname_length,
		     kind, rtype, rtype_length,
		     have_arg_list, file_seen, line_seen)
     char *fname;
     int fname_length;
     int kind; /* One of 'f' 'F' or 'I' */
     char *rtype;
     int rtype_length;
     int have_arg_list;
     char *file_seen;
     int line_seen;
{
  struct partial_proto *partial;
  int i;
  struct fn_decl *fn;
#if ADD_MISSING_EXTERN_C
  if (kind == 'f')
    missing_extern_C_count++;
#endif

  fn = lookup_std_proto (fname, fname_length);

  /* Remove the function from the list of required function. */
  if (fn)
    {
      if (REQUIRED (fn))
	required_unseen_count--;
      SET_SEEN (fn);
    }

  /* If we have a full prototype, we're done. */
  if (have_arg_list)
    return;
      
  if (kind == 'I')  /* don't edit inline function */
    return;

  /* If the partial prototype was included from some other file,
     we don't need to patch it up (in this run). */
  i = strlen (file_seen);
  if (i < inc_filename_length
      || strcmp (inc_filename, file_seen + (i - inc_filename_length)) != 0)
    return;

  if (fn == NULL)
    return;
  if (fn->params[0] == '\0' || strcmp (fn->params, "void") == 0)
    return;

  /* We only have a partial function declaration,
     so remember that we have to add a complete prototype. */
  partial_count++;
  partial = (struct partial_proto*)
    obstack_alloc (&scan_file_obstack, sizeof (struct partial_proto));
  partial->fname = obstack_alloc (&scan_file_obstack, fname_length + 1);
  bcopy (fname, partial->fname, fname_length);
  partial->fname[fname_length] = 0;
  partial->rtype = obstack_alloc (&scan_file_obstack, rtype_length + 1);
  sprintf (partial->rtype, "%.*s", rtype_length, rtype);
  partial->line_seen = line_seen;
  partial->fn = fn;
  fn->partial = partial;
  partial->next = partial_proto_list;
  partial_proto_list = partial;
  if (verbose)
    {
      fprintf (stderr, "(%s: %s non-prototype function declaration.)\n",
	       inc_filename, partial->fname);
    }
}

/* For any name in NAMES that is defined as a macro,
   call recognized_macro on it. */

void
check_macro_names (pfile, names)
     struct parse_file *pfile;
     namelist names;
{
  while (*names)
    {
      if (cpp_lookup (pfile, names, -1, -1))
	recognized_macro (names);
      names += strlen (names) + 1;
    }
}

void
read_scan_file (in_fname, argc, argv)
     char *in_fname;
     int argc;
     char **argv;
{
  cpp_reader scan_in;
  cpp_options scan_options;
  struct fn_decl *fn;
  int i;

  obstack_init (&scan_file_obstack); 

  init_parse_file (&scan_in);
  scan_in.data = &scan_options;
  init_parse_options (&scan_options);
  i = cpp_handle_options (&scan_in, argc, argv);
  if (i < argc)
    fatal ("Invalid option `%s'", argv[i]);
  push_parse_file (&scan_in, in_fname);
  CPP_OPTIONS (&scan_in)->no_line_commands = 1;

  scan_decls (&scan_in, argc, argv);
  check_macro_names (&scan_in, include_entry->required);
  check_macro_names (&scan_in, include_entry->extra);

  if (verbose && (scan_in.errors + warnings) > 0)
    fprintf (stderr, "(%s: %d errors and %d warnings from cpp)\n",
	     inc_filename, scan_in.errors, warnings);
  if (scan_in.errors)
    exit (0);

  /* Traditionally, getc and putc are defined in terms of _filbuf and _flsbuf.
     If so, those functions are also required. */
  if (special_file_handling == stdio_h
      && (fn = lookup_std_proto ("_filbuf", 7)) != NULL)
    {
      static char getchar_call[] = "getchar();";
      cpp_buffer *buf =
	cpp_push_buffer (&scan_in, getchar_call, sizeof(getchar_call) - 1);
      int old_written = CPP_WRITTEN (&scan_in);
      int seen_filbuf = 0;

      /* Scan the macro expansion of "getchar();". */
      for (;;)
	{
	  enum cpp_token token = cpp_get_token (&scan_in);
	  int length = CPP_WRITTEN (&scan_in) - old_written;
	  CPP_SET_WRITTEN (&scan_in, old_written);
	  if (token == CPP_EOF) /* Should not happen ... */
	    break;
	  if (token == CPP_POP && CPP_BUFFER (&scan_in) == buf)
	    {
	      cpp_pop_buffer (&scan_in);
	      break;
	    }
	  if (token == CPP_NAME && length == 7
	      && strcmp ("_filbuf", scan_in.token_buffer + old_written) == 0)
	    seen_filbuf++;
	}
      if (seen_filbuf)
	{
	  int need_filbuf = !SEEN (fn) && !REQUIRED (fn);
	  struct fn_decl *flsbuf_fn = lookup_std_proto ("_flsbuf", 7);
	  int need_flsbuf
	    = flsbuf_fn && !SEEN (flsbuf_fn) && !REQUIRED (flsbuf_fn);

	  /* Append "_filbuf" and/or "_flsbuf" to end of
	     required_functions_list. */
	  if (need_filbuf + need_flsbuf)
	    {
	      int old_len = namelist_end (required_functions_list)
		- required_functions_list;
	      char *new_list = (char*) xmalloc (old_len + 20);
	      bcopy (required_functions_list, new_list, old_len);
	      if (need_filbuf)
		{
		  strcpy (new_list + old_len, "_filbuf");
		  old_len += 8;
		  SET_REQUIRED (fn);
		}
	      if (need_flsbuf)
		{
		  strcpy (new_list + old_len, "_flsbuf");
		  old_len += 8;
		  SET_REQUIRED (flsbuf_fn);
		}
	      new_list[old_len] = '\0';
	      required_functions_list = (namelist)new_list;
	      required_unseen_count += need_filbuf + need_flsbuf;
	    }
	}
    }

  if (required_unseen_count + partial_count + missing_errno
#if ADD_MISSING_EXTERN_C
      + missing_extern_C_count
#endif      
      == 0)
    {
      if (verbose)
	fprintf (stderr, "%s: OK, nothing needs to be done.\n", inc_filename);
      exit (0);
    }
  if (!verbose)
    fprintf (stderr, "%s: fixing %s\n", progname, inc_filename);
  else
    {
      if (required_unseen_count)
	fprintf (stderr, "%s: %d missing function declarations.\n",
		 inc_filename, required_unseen_count);
      if (partial_count)
	fprintf (stderr, "%s: %d non-prototype function declarations.\n",
		 inc_filename, partial_count);
#if ADD_MISSING_EXTERN_C
      if (missing_extern_C_count)
	fprintf (stderr,
		 "%s: %d declarations not protected by extern \"C\".\n",
		 inc_filename, missing_extern_C_count);
#endif
    }
}

void
write_rbrac ()
{
  struct fn_decl *fn;
  const char *cptr;

  if (required_unseen_count)
    {
      fprintf (outf,
	"#if defined(__cplusplus) || defined(__USE_FIXED_PROTOTYPES__)\n");
#ifdef NO_IMPLICIT_EXTERN_C
      fprintf (outf, "#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
#endif
    }

  /* Now we print out prototypes for those functions that we haven't seen. */
  for (cptr = required_functions_list; *cptr!= '\0'; )
    {
      int macro_protect = 0;
      int name_len = strlen (cptr);

      fn = lookup_std_proto (cptr, name_len);
      cptr+= name_len + 1;
      if (fn == NULL || !REQUIRED (fn))
	continue;

      /* In the case of memmove, protect in case the application
	 defines it as a macro before including the header.  */
      if (!strcmp (fn->fname, "memmove")
	  || !strcmp (fn->fname, "vprintf")
	  || !strcmp (fn->fname, "vfprintf")
	  || !strcmp (fn->fname, "vsprintf")
	  || !strcmp (fn->fname, "rewinddir"))
	macro_protect = 1;

      if (macro_protect)
	fprintf (outf, "#ifndef %s\n", fn->fname);
      fprintf (outf, "extern %s %s (%s);\n",
	       fn->rtype, fn->fname, fn->params);
      if (macro_protect)
	fprintf (outf, "#endif\n");
    }
  if (required_unseen_count)
    {
#ifdef NO_IMPLICIT_EXTERN_C
      fprintf (outf, "#ifdef __cplusplus\n}\n#endif\n");
#endif
      fprintf (outf,
	"#endif /* defined(__cplusplus) || defined(__USE_FIXED_PROTOTYPES__*/\n");
    }

  switch (special_file_handling)
    {
    case errno_h:
      if (missing_errno)
	fprintf (outf, "extern int errno;\n");
      break;
    case sys_stat_h:
      if (!seen_S_ISBLK && seen_S_IFBLK)
	fprintf (outf,
		 "#define S_ISBLK(mode) (((mode) & S_IFMT) == S_IFBLK)\n");
      if (!seen_S_ISCHR && seen_S_IFCHR)
	fprintf (outf,
		 "#define S_ISCHR(mode) (((mode) & S_IFMT) == S_IFCHR)\n");
      if (!seen_S_ISDIR && seen_S_IFDIR)
	fprintf (outf,
		 "#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)\n");
      if (!seen_S_ISFIFO && seen_S_IFIFO)
	fprintf (outf,
		 "#define S_ISFIFO(mode) (((mode) & S_IFMT) == S_IFIFO)\n");
      if (!seen_S_ISLNK && seen_S_IFLNK)
	fprintf (outf,
		 "#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)\n");
      if (!seen_S_ISREG && seen_S_IFREG)
	fprintf (outf,
		 "#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)\n");
      break;
    }


#if ADD_MISSING_EXTERN_C
  if (missing_extern_C_count + required_unseen_count > 0)
    fprintf (outf, "#ifdef __cplusplus\n}\n#endif\n");
#endif
}

char *
xstrdup (str)
     char *str;
{
  char *copy = (char *) xmalloc (strlen (str) + 1);
  strcpy (copy, str);
  return copy;
}

/* Returns 1 iff the file is properly protected from multiple inclusion:
   #ifndef PROTECT_NAME
   #define PROTECT_NAME
   #endif

 */

#define INF_GET() (inf_ptr < inf_limit ? *(unsigned char*)inf_ptr++ : EOF)
#define INF_UNGET(c) ((c)!=EOF && inf_ptr--)

int
inf_skip_spaces (c)
     int c;
{
  for (;;)
    {
      if (c == ' ' || c == '\t')
	c = INF_GET ();
      else if (c == '/')
	{
	  c = INF_GET ();
	  if (c != '*')
	    {
	      INF_UNGET (c);
	      return '/';
	    }
	  c = INF_GET ();
	  for (;;)
	    {
	      if (c == EOF)
		return EOF;
	      else if (c != '*')
		{
		  if (c == '\n')
		    source_lineno++, lineno++;
		  c = INF_GET ();
		}
	      else if ((c = INF_GET ()) == '/')
		return INF_GET ();
	    }
	}
      else
	break;
    }
  return c;
}

/* Read into STR from inf_buffer upto DELIM. */

int
inf_read_upto (str, delim)
     sstring *str;
     int delim;
{
  int ch;
  for (;;)
    {
      ch = INF_GET ();
      if (ch == EOF || ch == delim)
	break;
      SSTRING_PUT (str, ch);
    }
  MAKE_SSTRING_SPACE (str, 1);
  *str->ptr = 0;
  return ch;
}

int
inf_scan_ident (s, c)
     register sstring *s;
     int c;
{
  s->ptr = s->base;
  if (isalpha (c) || c == '_')
    {
      for (;;)
	{
	  SSTRING_PUT (s, c);
	  c = INF_GET ();
	  if (c == EOF || !(isalnum (c) || c == '_'))
	    break;
	}
    }
  MAKE_SSTRING_SPACE (s, 1);
  *s->ptr = 0;
  return c;
}

/* Returns 1 if the file is correctly protected against multiple
   inclusion, setting *ifndef_line to the line number of the initial #ifndef
   and setting *endif_line to the final #endif.
   Otherwise return 0. */

int
check_protection (ifndef_line, endif_line)
     int *ifndef_line, *endif_line;
{
  int c;
  int if_nesting = 1; /* Level of nesting of #if's */
  char *protect_name = NULL; /* Identifier following initial #ifndef */
  int define_seen = 0;

  /* Skip initial white space (including comments). */
  for (;; lineno++)
    {
      c = inf_skip_spaces (' ');
      if (c == EOF)
	return 0;
      if (c != '\n')
	break;
    }
  if (c != '#')
    return 0;
  c = inf_scan_ident (&buf, inf_skip_spaces (' '));
  if (SSTRING_LENGTH (&buf) == 0 || strcmp (buf.base, "ifndef") != 0)
    return 0;

  /* So far so good: We've seen an initial #ifndef. */
  *ifndef_line = lineno;
  c = inf_scan_ident (&buf, inf_skip_spaces (c));
  if (SSTRING_LENGTH (&buf) == 0 || c == EOF)
    return 0;
  protect_name = xstrdup (buf.base);

  INF_UNGET (c);
  c = inf_read_upto (&buf, '\n');
  if (c == EOF)
    return 0;
  lineno++;

  for (;;)
    {
      c = inf_skip_spaces (' ');
      if (c == EOF)
	return 0;
      if (c == '\n')
	{
	  lineno++;
	  continue;
	}
      if (c != '#')
	goto skip_to_eol;
      c = inf_scan_ident (&buf, inf_skip_spaces (' '));
      if (SSTRING_LENGTH (&buf) == 0)
	;
      else if (!strcmp (buf.base, "ifndef")
	  || !strcmp (buf.base, "ifdef") || !strcmp (buf.base, "if"))
	{
	  if_nesting++;
	}
      else if (!strcmp (buf.base, "endif"))
	{
	  if_nesting--;
	  if (if_nesting == 0)
	    break;
	}
      else if (!strcmp (buf.base, "else"))
	{
	  if (if_nesting == 1)
	    return 0;
	}
      else if (!strcmp (buf.base, "define"))
	{
	  if (if_nesting != 1)
	    goto skip_to_eol;
	  c = inf_skip_spaces (c);
	  c = inf_scan_ident (&buf, c);
	  if (buf.base[0] > 0 && strcmp (buf.base, protect_name) == 0)
	    define_seen = 1;
	}
    skip_to_eol:
      for (;;)
	{
	  if (c == '\n' || c == EOF)
	    break;
	  c = INF_GET ();
	}
      if (c == EOF)
	return 0;
      lineno++;
    }

  if (!define_seen)
     return 0;
  *endif_line = lineno;
  /* Skip final white space (including comments). */
  for (;;)
    {
      c = inf_skip_spaces (' ');
      if (c == EOF)
	break;
      if (c != '\n')
	return 0;
    }

  return 1;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int inf_fd;
  struct stat sbuf;
  int c;
  int i, done;
  const char *cptr, **pptr;
  int ifndef_line;
  int endif_line;
  long to_read;
  long int inf_size;

  if (argv[0] && argv[0][0])
    {
      register char *p;

      progname = 0;
      for (p = argv[0]; *p; p++)
        if (*p == '/')
          progname = p;
      progname = progname ? progname+1 : argv[0];
    }

  if (argc < 4)
    {
      fprintf (stderr, "%s: Usage: foo.h infile.h outfile.h options\n",
	       progname);
      exit (-1);
    }

  inc_filename = argv[1];
  inc_filename_length = strlen (inc_filename);

#ifdef FIXPROTO_IGNORE_LIST
  for (i = 0; files_to_ignore[i] != NULL; i++)
    {
      char *ignore_name = files_to_ignore[i];
      int ignore_len = strlen (ignore_name);
      if (strncmp (inc_filename, ignore_name, ignore_len) == 0)
	{
	  if (ignore_name[ignore_len-1] == '/'
	      || inc_filename[ignore_len] == '\0')
	    {
	      if (verbose)
		fprintf (stderr, "%s: ignoring %s\n", progname, inc_filename);
	      exit (0);
	    }
	}
	  
    }
#endif

  if (strcmp (inc_filename, "sys/stat.h") == 0)
    special_file_handling = sys_stat_h;
  else if (strcmp (inc_filename, "errno.h") == 0)
    special_file_handling = errno_h, missing_errno = 1;
  else if (strcmp (inc_filename, "stdio.h") == 0)
    special_file_handling = stdio_h;
  include_entry = std_include_table;
  while (include_entry->name != NULL
	 && strcmp (inc_filename, include_entry->name) != 0)
    include_entry++;

  required_functions_list = include_entry->required;

  /* Count and mark the prototypes required for this include file. */ 
  for (cptr = required_functions_list; *cptr!= '\0'; )
    {
      int name_len = strlen (cptr);
      struct fn_decl *fn = lookup_std_proto (cptr, name_len);
      required_unseen_count++;
      if (fn == NULL)
	fprintf (stderr, "Internal error:  No prototype for %s\n", cptr);
      else
	SET_REQUIRED (fn);
      cptr += name_len + 1;
    }

  read_scan_file (argv[2], argc - 4, argv + 4);

  inf_fd = open (argv[2], O_RDONLY, 0666);
  if (inf_fd < 0)
    {
      fprintf (stderr, "%s: Cannot open '%s' for reading -",
	       progname, argv[2]);
      perror (NULL);
      exit (-1);
    }
  if (fstat (inf_fd, &sbuf) < 0)
    {
      fprintf (stderr, "%s: Cannot get size of '%s' -", progname, argv[2]);
      perror (NULL);
      exit (-1);
    }
  inf_size = sbuf.st_size;
  inf_buffer = (char*) xmalloc (inf_size + 2);
  inf_buffer[inf_size] = '\n';
  inf_buffer[inf_size + 1] = '\0';
  inf_limit = inf_buffer + inf_size;
  inf_ptr = inf_buffer;

  to_read = inf_size;
  while (to_read > 0)
    {
      long i = read (inf_fd, inf_buffer + inf_size - to_read, to_read);
      if (i < 0)
	{
	  fprintf (stderr, "%s: Failed to read '%s' -", progname, argv[2]);
	  perror (NULL);
	  exit (-1);
	}
      if (i == 0)
	{
	  inf_size -= to_read;
	  break;
	}
      to_read -= i;
    }

  close (inf_fd);

  /* If file doesn't end with '\n', add one. */
  if (inf_limit > inf_buffer && inf_limit[-1] != '\n')
    inf_limit++;

  unlink (argv[3]);
  outf = fopen (argv[3], "w");
  if (outf == NULL)
    {
      fprintf (stderr, "%s: Cannot open '%s' for writing -",
	       progname, argv[3]);
      perror (NULL);
      exit (-1);
    }

  lineno = 1;

  if (check_protection (&ifndef_line, &endif_line))
    {
      lbrac_line = ifndef_line+1;
      rbrac_line = endif_line;
    }
  else
    {
      lbrac_line = 1;
      rbrac_line = -1;
    }

  /* Reset input file. */
  inf_ptr = inf_buffer;
  lineno = 1;

  for (;;)
    {
      if (lineno == lbrac_line)
	write_lbrac ();
      if (lineno == rbrac_line)
	write_rbrac ();
      for (;;)
	{
	  struct fn_decl *fn;
	  c = INF_GET ();
	  if (c == EOF)
	    break;
	  if (isalpha (c) || c == '_')
	    {
	      c = inf_scan_ident (&buf, c);
	      INF_UNGET (c);
	      fputs (buf.base, outf);
	      fn = lookup_std_proto (buf.base, strlen (buf.base));
	      /* We only want to edit the declaration matching the one
		 seen by scan-decls, as there can be multiple
		 declarations, selected by #ifdef __STDC__ or whatever. */
	      if (fn && fn->partial && fn->partial->line_seen == lineno)
		{
		  c = inf_skip_spaces (' ');
		  if (c == EOF)
		    break;
		  if (c == '(')
		    {
		      c = inf_skip_spaces (' ');
		      if (c == ')')
			{
			  fprintf (outf, " _PARAMS((%s))", fn->params);
			}
		      else
			{
			  putc ('(', outf);
			  INF_UNGET (c);
			}
		    }
		  else
		    fprintf (outf, " %c", c);
		}
	    }
	  else
	    {
	      putc (c, outf);
	      if (c == '\n')
		break;
	    }
	}
      if (c == EOF)
	break;
      lineno++;
    }
  if (rbrac_line < 0)
    write_rbrac ();

  fclose (outf);

  return 0;
}

/* Stub error functions.  These replace cpperror.c,
   because we want to suppress error messages. */

void
cpp_file_line_for_message (pfile, filename, line, column)
     cpp_reader *pfile;
     char *filename;
     int line, column;
{
  if (!verbose)
    return;
  if (column > 0)
    fprintf (stderr, "%s:%d:%d: ", filename, line, column);
  else
    fprintf (stderr, "%s:%d: ", filename, line);
}

void
cpp_print_containing_files (pfile)
     cpp_reader *pfile;
{
}

/* IS_ERROR is 1 for error, 0 for warning */
void cpp_message (pfile, is_error, msg, arg1, arg2, arg3)
     int is_error;
     cpp_reader *pfile;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  if (is_error)
    pfile->errors++;
  if (!verbose)
    return;
  if (!is_error)
    fprintf (stderr, "warning: ");
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
}

void
fatal (str, arg)
     char *str, *arg;
{
  fprintf (stderr, "%s: %s: ", progname, inc_filename);
  fprintf (stderr, str, arg);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

void
cpp_pfatal_with_name (pfile, name)
     cpp_reader *pfile;
     char *name;
{
  cpp_perror_with_name (pfile, name);
  exit (FATAL_EXIT_CODE);
}
