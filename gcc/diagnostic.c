/* Top level of GNU C compiler
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */


/* This file implements the language independant aspect of diagnostic
   message module.  */

#include "config.h"
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"

#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "input.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "insn-config.h"
#include "toplev.h"
#include "intl.h"


/* Prototypes. */
static void notice PVPROTO((const char *s, ...)) ATTRIBUTE_PRINTF_1;
static void vmessage PROTO((const char *, const char *, va_list));
static void v_message_with_file_and_line PROTO((const char *, int, int,
						const char *, va_list));
static void v_message_with_decl PROTO((tree, int, const char *, va_list));
static void file_and_line_for_asm PROTO((rtx, char **, int *));
static void v_error_with_file_and_line PROTO((const char *, int,
					      const char *, va_list));
static void v_error_with_decl PROTO((tree, const char *, va_list));
static void v_error_for_asm PROTO((rtx, const char *, va_list));
static void verror PROTO((const char *, va_list));
static void vfatal PROTO((const char *, va_list)) ATTRIBUTE_NORETURN;
static void v_warning_with_file_and_line PROTO ((const char *, int,
						 const char *, va_list));
static void v_warning_with_decl PROTO((tree, const char *, va_list));
static void v_warning_for_asm PROTO((rtx, const char *, va_list));
static void vwarning PROTO((const char *, va_list));
static void vpedwarn PROTO((const char *, va_list));
static void v_pedwarn_with_decl PROTO((tree, const char *, va_list));
static void v_pedwarn_with_file_and_line PROTO((const char *, int,
						const char *, va_list));
static void vsorry PROTO((const char *, va_list));
static void report_file_and_line PROTO ((const char *, int, int));
static void vnotice PROTO ((FILE *, const char *, va_list));


extern int rtl_dump_and_exit;
extern int inhibit_warnings;
extern int warnings_are_errors;
extern int warningcount;
extern int errorcount;

static int need_error_newline;

/* Function of last error message;
   more generally, function such that if next error message is in it
   then we don't have to mention the function name.  */
static tree last_error_function = NULL;

/* Used to detect when input_file_stack has changed since last described.  */
static int last_error_tick;

/* Called by report_error_function to print out function name.
 * Default may be overridden by language front-ends.  */

void (*print_error_function) PROTO((const char *)) =
  default_print_error_function;


/* Print the message MSGID in FILE.  */

static void
vnotice (file, msgid, ap)
     FILE *file;
     const char *msgid;
     va_list ap;
{
  vfprintf (file, _(msgid), ap);
}

/* Print MSGID on stderr.  */

static void
notice VPROTO((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, char *);
#endif

  vnotice (stderr, msgid, ap);
  va_end (ap);
}

/* Report FILE and LINE (or program name), and optionally just WARN.  */

static void
report_file_and_line (file, line, warn)
     const char *file;
     int line;
     int warn;
{
  if (file)
    fprintf (stderr, "%s:%d: ", file, line);
  else
    fprintf (stderr, "%s: ", progname);

  if (warn)
    notice ("warning: ");
}

/* Print a PREFIXed MSGID.  */

static void
vmessage (prefix, msgid, ap)
     const char *prefix;
     const char *msgid;
     va_list ap;
{
  if (prefix)
    fprintf (stderr, "%s: ", prefix);

  vfprintf (stderr, msgid, ap);
}

/* Print a message relevant to line LINE of file FILE.  */

static void
v_message_with_file_and_line (file, line, warn, msgid, ap)
     const char *file;
     int line;
     int warn;
     const char *msgid;
     va_list ap;
{
  report_file_and_line (file, line, warn);
  vnotice (stderr, msgid, ap);
  fputc ('\n', stderr);
}

/* Print a message relevant to the given DECL.  */

static void
v_message_with_decl (decl, warn, msgid, ap)
     tree decl;
     int warn;
     const char *msgid;
     va_list ap;
{
  const char *p;

  report_file_and_line (DECL_SOURCE_FILE (decl),
			DECL_SOURCE_LINE (decl), warn);

  /* Do magic to get around lack of varargs support for insertion
     of arguments into existing list.  We know that the decl is first;
     we ass_u_me that it will be printed with "%s".  */

  for (p = _(msgid); *p; ++p)
    {
      if (*p == '%')
	{
	  if (*(p + 1) == '%')
	    ++p;
	  else if (*(p + 1) != 's')
	    abort ();
	  else
	    break;
	}
    }

  if (p > _(msgid))			/* Print the left-hand substring.  */
    fprintf (stderr, "%.*s", (int)(p - _(msgid)), _(msgid));

  if (*p == '%')		/* Print the name.  */
    {
      const char *n = (DECL_NAME (decl)
		 ? (*decl_printable_name) (decl, 2)
		 : "((anonymous))");
      fputs (n, stderr);
      while (*p)
	{
	  ++p;
	  if (ISALPHA (*(p - 1) & 0xFF))
	    break;
	}
    }

  if (*p)			/* Print the rest of the message.  */
    vmessage ((char *)NULL, p, ap);

  fputc ('\n', stderr);
}

/* Figure file and line of the given INSN.  */

static void
file_and_line_for_asm (insn, pfile, pline)
     rtx insn;
     char **pfile;
     int *pline;
{
  rtx body = PATTERN (insn);
  rtx asmop;

  /* Find the (or one of the) ASM_OPERANDS in the insn.  */
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    asmop = SET_SRC (body);
  else if (GET_CODE (body) == ASM_OPERANDS)
    asmop = body;
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    asmop = SET_SRC (XVECEXP (body, 0, 0));
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    asmop = XVECEXP (body, 0, 0);
  else
    asmop = NULL;

  if (asmop)
    {
      *pfile = ASM_OPERANDS_SOURCE_FILE (asmop);
      *pline = ASM_OPERANDS_SOURCE_LINE (asmop);
    }
  else
    {
      *pfile = input_filename;
      *pline = lineno;
    }
}

/* Report an error at line LINE of file FILE.  */

static void
v_error_with_file_and_line (file, line, msgid, ap)
     const char *file;
     int line;
     const char *msgid;
     va_list ap;
{
  count_error (0);
  report_error_function (file);
  v_message_with_file_and_line (file, line, 0, msgid, ap);
}

/* Report an error at the declaration DECL.
   MSGID is a format string which uses %s to substitute the declaration
   name; subsequent substitutions are a la printf.  */

static void
v_error_with_decl (decl, msgid, ap)
     tree decl;
     const char *msgid;
     va_list ap;
{
  count_error (0);
  report_error_function (DECL_SOURCE_FILE (decl));
  v_message_with_decl (decl, 0, msgid, ap);
}


/* Report an error at the line number of the insn INSN.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

static void
v_error_for_asm (insn, msgid, ap)
     rtx insn;
     const char *msgid;
     va_list ap;
{
  char *file;
  int line;

  count_error (0);
  file_and_line_for_asm (insn, &file, &line);
  report_error_function (file);
  v_message_with_file_and_line (file, line, 0, msgid, ap);
}


/* Report an error at the current line number.  */

static void
verror (msgid, ap)
     const char *msgid;
     va_list ap;
{
  v_error_with_file_and_line (input_filename, lineno, msgid, ap);
}


/* Report a fatal error at the current line number.  Allow a front end to
   intercept the message.  */

static void (*fatal_function) PROTO ((const char *, va_list));

static void
vfatal (msgid, ap)
     const char *msgid;
     va_list ap;
{
   if (fatal_function != 0)
     (*fatal_function) (_(msgid), ap);

  verror (msgid, ap);
  exit (FATAL_EXIT_CODE);
}

/* Report a warning at line LINE of file FILE.  */

static void
v_warning_with_file_and_line (file, line, msgid, ap)
     const char *file;
     int line;
     const char *msgid;
     va_list ap;
{
  if (count_error (1))
    {
      report_error_function (file);
      v_message_with_file_and_line (file, line, 1, msgid, ap);
    }
}


/* Report a warning at the declaration DECL.
   MSGID is a format string which uses %s to substitute the declaration
   name; subsequent substitutions are a la printf.  */

static void
v_warning_with_decl (decl, msgid, ap)
     tree decl;
     const char *msgid;
     va_list ap;
{
  if (count_error (1))
    {
      report_error_function (DECL_SOURCE_FILE (decl));
      v_message_with_decl (decl, 1, msgid, ap);
    }
}


/* Report a warning at the line number of the insn INSN.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

static void
v_warning_for_asm (insn, msgid, ap)
     rtx insn;
     const char *msgid;
     va_list ap;
{
  if (count_error (1))
    {
      char *file;
      int line;

      file_and_line_for_asm (insn, &file, &line);
      report_error_function (file);
      v_message_with_file_and_line (file, line, 1, msgid, ap);
    }
}


/* Report a warning at the current line number.  */

static void
vwarning (msgid, ap)
     const char *msgid;
     va_list ap;
{
  v_warning_with_file_and_line (input_filename, lineno, msgid, ap);
}

/* These functions issue either warnings or errors depending on
   -pedantic-errors.  */

static void
vpedwarn (msgid, ap)
     const char *msgid;
     va_list ap;
{
  if (flag_pedantic_errors)
    verror (msgid, ap);
  else
    vwarning (msgid, ap);
}


static void
v_pedwarn_with_decl (decl, msgid, ap)
     tree decl;
     const char *msgid;
     va_list ap;
{
  /* We don't want -pedantic-errors to cause the compilation to fail from
     "errors" in system header files.  Sometimes fixincludes can't fix what's
     broken (eg: unsigned char bitfields - fixing it may change the alignment
     which will cause programs to mysteriously fail because the C library
     or kernel uses the original layout).  There's no point in issuing a
     warning either, it's just unnecessary noise.  */

  if (! DECL_IN_SYSTEM_HEADER (decl))
    {
      if (flag_pedantic_errors)
	v_error_with_decl (decl, msgid, ap);
      else
	v_warning_with_decl (decl, msgid, ap);
    }
}


static void
v_pedwarn_with_file_and_line (file, line, msgid, ap)
     const char *file;
     int line;
     const char *msgid;
     va_list ap;
{
  if (flag_pedantic_errors)
    v_error_with_file_and_line (file, line, msgid, ap);
  else
    v_warning_with_file_and_line (file, line, msgid, ap);
}


/* Apologize for not implementing some feature.  */

static void
vsorry (msgid, ap)
     const char *msgid;
     va_list ap;
{
  sorrycount++;
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);
  notice ("sorry, not implemented: ");
  vnotice (stderr, msgid, ap);
  fputc ('\n', stderr);
}


/* Count an error or warning.  Return 1 if the message should be printed.  */

int
count_error (warningp)
     int warningp;
{
  if (warningp && inhibit_warnings)
    return 0;

  if (warningp && !warnings_are_errors)
    warningcount++;
  else
    {
      static int warning_message = 0;

      if (warningp && !warning_message)
	{
	  notice ("%s: warnings being treated as errors\n", progname);
	  warning_message = 1;
	}
      errorcount++;
    }

  return 1;
}

/* Print a diagnistic MSGID on FILE.  */
void
fnotice VPROTO((FILE *file, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  FILE *file;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  file = va_arg (ap, FILE *);
  msgid = va_arg (ap, const char *);
#endif

  vnotice (file, msgid, ap);
  va_end (ap);
}


/* Print a fatal error message.  NAME is the text.
   Also include a system error message based on `errno'.  */

void
pfatal_with_name (name)
  const char *name;
{
  fprintf (stderr, "%s: ", progname);
  perror (name);
  exit (FATAL_EXIT_CODE);
}

void
fatal_io_error (name)
  const char *name;
{
  notice ("%s: %s: I/O error\n", progname, name);
  exit (FATAL_EXIT_CODE);
}

/* Issue a pedantic warning MSGID.  */
void
pedwarn VPROTO((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  vpedwarn (msgid, ap);
  va_end (ap);
}

/* Issue a pedantic waring about DECL.  */
void
pedwarn_with_decl VPROTO((tree decl, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  tree decl;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  decl = va_arg (ap, tree);
  msgid = va_arg (ap, const char *);
#endif

  v_pedwarn_with_decl (decl, msgid, ap);
  va_end (ap);
}

/* Same as above but within the context FILE and LINE. */
void
pedwarn_with_file_and_line VPROTO((const char *file, int line,
				   const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *file;
  int line;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  file = va_arg (ap, const char *);
  line = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  v_pedwarn_with_file_and_line (file, line, msgid, ap);
  va_end (ap);
}

/* Just apologize with MSGID.  */
void
sorry VPROTO((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  vsorry (msgid, ap);
  va_end (ap);
}

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */

void
announce_function (decl)
     tree decl;
{
  if (! quiet_flag)
    {
      if (rtl_dump_and_exit)
	fprintf (stderr, "%s ", IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	fprintf (stderr, " %s", (*decl_printable_name) (decl, 2));
      fflush (stderr);
      need_error_newline = 1;
      last_error_function = current_function_decl;
    }
}

/* The default function to print out name of current function that caused
   an error.  */

void
default_print_error_function (file)
  const char *file;
{
  if (last_error_function != current_function_decl)
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      if (current_function_decl == NULL)
	notice ("At top level:\n");
      else
	{
	  if (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
	    notice ("In method `%s':\n",
		    (*decl_printable_name) (current_function_decl, 2));
	  else
	    notice ("In function `%s':\n",
		    (*decl_printable_name) (current_function_decl, 2));
	}

      last_error_function = current_function_decl;
    }
}

/* Prints out, if necessary, the name of the current function
  that caused an error.  Called from all error and warning functions.
  We ignore the FILE parameter, as it cannot be relied upon.  */

void
report_error_function (file)
  const char *file ATTRIBUTE_UNUSED;
{
  struct file_stack *p;

  if (need_error_newline)
    {
      fprintf (stderr, "\n");
      need_error_newline = 0;
    }

  if (input_file_stack && input_file_stack->next != 0
      && input_file_stack_tick != last_error_tick)
    {
      for (p = input_file_stack->next; p; p = p->next)
	if (p == input_file_stack->next)
	  notice ("In file included from %s:%d", p->name, p->line);
	else
	  notice (",\n                 from %s:%d", p->name, p->line);
      fprintf (stderr, ":\n");
      last_error_tick = input_file_stack_tick;
    }

  (*print_error_function) (input_filename);
}

void
error_with_file_and_line VPROTO((const char *file, int line,
				 const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *file;
  int line;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  file = va_arg (ap, const char *);
  line = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  v_error_with_file_and_line (file, line, msgid, ap);
  va_end (ap);
}

void
error_with_decl VPROTO((tree decl, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  tree decl;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  decl = va_arg (ap, tree);
  msgid = va_arg (ap, const char *);
#endif

  v_error_with_decl (decl, msgid, ap);
  va_end (ap);
}

void
error_for_asm VPROTO((rtx insn, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  rtx insn;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  insn = va_arg (ap, rtx);
  msgid = va_arg (ap, const char *);
#endif

  v_error_for_asm (insn, msgid, ap);
  va_end (ap);
}

void
error VPROTO((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  verror (msgid, ap);
  va_end (ap);
}

/* Set the function to call when a fatal error occurs.  */

void
set_fatal_function (f)
     void (*f) PROTO ((const char *, va_list));
{
  fatal_function = f;
}

void
fatal VPROTO((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  vfatal (msgid, ap);
  va_end (ap);
}

void
_fatal_insn (msgid, insn, file, line, function)
     const char *msgid;
     rtx insn;
     const char *file;
     int line;
     const char *function;
{
  error ("%s", msgid);
  debug_rtx (insn);
  fancy_abort (file, line, function);
}

void
_fatal_insn_not_found (insn, file, line, function)
     rtx insn;
     const char *file;
     int line;
     const char *function;
{
  if (INSN_CODE (insn) < 0)
    _fatal_insn ("Unrecognizable insn:", insn, file, line, function);
  else
    _fatal_insn ("Insn does not satisfy its constraints:",
		insn, file, line, function);
}

void
warning_with_file_and_line VPROTO((const char *file, int line,
				   const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *file;
  int line;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  file = va_arg (ap, const char *);
  line = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  v_warning_with_file_and_line (file, line, msgid, ap);
  va_end (ap);
}

void
warning_with_decl VPROTO((tree decl, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  tree decl;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  decl = va_arg (ap, tree);
  msgid = va_arg (ap, const char *);
#endif

  v_warning_with_decl (decl, msgid, ap);
  va_end (ap);
}

void
warning_for_asm VPROTO((rtx insn, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  rtx insn;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  insn = va_arg (ap, rtx);
  msgid = va_arg (ap, const char *);
#endif

  v_warning_for_asm (insn, msgid, ap);
  va_end (ap);
}

void
warning VPROTO((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  vwarning (msgid, ap);
  va_end (ap);
}

