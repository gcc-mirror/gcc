/* Top level of GNU C compiler
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free

struct output_buffer
{
  struct obstack obstack;       /* where we build the text to output */
  char *prefix;                 /* prefix of every new line  */
  int line_length;              /* current line length (in characters) */
  int max_length;               /* maximum characters per line */
};

/* Prototypes. */
static int doing_line_wrapping PARAMS ((void));
static void init_output_buffer PARAMS ((struct output_buffer*, char *, int));
static char *get_output_prefix PARAMS ((const struct output_buffer *));
static int output_space_left PARAMS ((const struct output_buffer *));
static void emit_output_prefix PARAMS ((struct output_buffer *));
static void output_newline PARAMS ((struct output_buffer *));
static void output_append PARAMS ((struct output_buffer *, const char *,
				   const char *));
static void output_puts PARAMS ((struct output_buffer *, const char *));
static void dump_output PARAMS ((struct output_buffer *, FILE *));
static char *vbuild_message_string PARAMS ((const char *, va_list));
static char *build_message_string PARAMS ((const char *, ...))
     ATTRIBUTE_PRINTF_1;
static char *build_location_prefix PARAMS ((const char *, int, int));
static void voutput_notice PARAMS ((struct output_buffer *, const char *,
				    va_list));
static void output_printf PARAMS ((struct output_buffer *, const char *, ...))
     ATTRIBUTE_PRINTF_2;
static void line_wrapper_printf PARAMS ((FILE *, const char *, ...))
     ATTRIBUTE_PRINTF_2;
static void vline_wrapper_message_with_location PARAMS ((const char *, int,
							 int, const char *,
							 va_list));
static void notice PARAMS ((const char *s, ...)) ATTRIBUTE_PRINTF_1;
static void v_message_with_file_and_line PARAMS ((const char *, int, int,
						  const char *, va_list));
static void v_message_with_decl PARAMS ((tree, int, const char *, va_list));
static void file_and_line_for_asm PARAMS ((rtx, const char **, int *));
static void v_error_with_file_and_line PARAMS ((const char *, int,
						const char *, va_list));
static void v_error_with_decl PARAMS ((tree, const char *, va_list));
static void v_error_for_asm PARAMS ((rtx, const char *, va_list));
static void verror PARAMS ((const char *, va_list));
static void vfatal PARAMS ((const char *, va_list)) ATTRIBUTE_NORETURN;
static void v_warning_with_file_and_line PARAMS ((const char *, int,
						  const char *, va_list));
static void v_warning_with_decl PARAMS ((tree, const char *, va_list));
static void v_warning_for_asm PARAMS ((rtx, const char *, va_list));
static void vwarning PARAMS ((const char *, va_list));
static void vpedwarn PARAMS ((const char *, va_list));
static void v_pedwarn_with_decl PARAMS ((tree, const char *, va_list));
static void v_pedwarn_with_file_and_line PARAMS ((const char *, int,
						  const char *, va_list));
static void vsorry PARAMS ((const char *, va_list));
static void report_file_and_line PARAMS ((const char *, int, int));
static void vnotice PARAMS ((FILE *, const char *, va_list));


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

void (*print_error_function) PARAMS ((const char *)) =
  default_print_error_function;

/* Maximum characters per line in automatic line wrapping mode.
   Zero means don't wrap lines. */

static int output_maximum_width = 0;

/* Predicate. Return 1 if we're in automatic line wrapping mode.  */

static int
doing_line_wrapping ()
{
  return output_maximum_width > 0;
}

/* Set Maximum characters per line in automatic line wrapping mode.  */

void
set_message_length (n)
     int n;
{
    output_maximum_width = n;
}

/* Construct an output BUFFER with PREFIX and of MAX_LENGTH characters
   per line.  */

static void
init_output_buffer (buffer, prefix, max_length)
     struct output_buffer *buffer;
     char *prefix;
     int max_length;
{
  int prefix_length = prefix == 0 ? 0 : strlen (prefix);

  obstack_init (&buffer->obstack);
  buffer->prefix = prefix;
  buffer->line_length = 0;
  /* If the prefix is ridiculously too long, output at least
     32 characters.  */
  if (max_length - prefix_length < 32)
    buffer->max_length = max_length + 32;
  else
    buffer->max_length = max_length;
}

/* Return BUFFER's prefix.  */

static char *
get_output_prefix (buffer)
     const struct output_buffer *buffer;
{
  return buffer->prefix;
}

/* Return the amount of characters BUFFER can accept to
   make a full line.  */

static int
output_space_left (buffer)
     const struct output_buffer *buffer;
{
  return buffer->max_length - buffer->line_length;
}

/* Dump BUFFER's prefix.  */

static void
emit_output_prefix (buffer)
     struct output_buffer *buffer;
{
  if (buffer->prefix)
    {
      buffer->line_length = strlen (buffer->prefix);
      obstack_grow (&buffer->obstack, buffer->prefix, buffer->line_length);
    }
}

/* Have BUFFER start a new line.  */

static void
output_newline (buffer)
     struct output_buffer *buffer;
{
  obstack_1grow (&buffer->obstack, '\n');
  buffer->line_length = 0;
}

/* Append a string deliminated by START and END to BUFFER.  No wrapping is
   done.  The caller must ensure that it is safe to do so.  */

static void
output_append (buffer, start, end)
     struct output_buffer *buffer;
     const char *start;
     const char *end;
{
  int n;

  /* Emit prefix and skip whitespace if we're starting a new line.  */
  if (buffer->line_length == 0)
    {
      emit_output_prefix (buffer);
      while (start != end && *start == ' ')
        ++start;
    }
  n = end - start;
  obstack_grow (&buffer->obstack, start, n);
  buffer->line_length += n;
}

/* Wrap a STRing into BUFFER.  */

static void
output_puts (buffer, str)
     struct output_buffer *buffer;
     const char *str;
{
  const char *p = str;
  
  while (*str)
    {
      while (*p && *p != ' ' && *p != '\n')
        ++p;
      
      if (p - str < output_space_left (buffer))
        output_append (buffer, str, p);
      else
        {
          output_newline (buffer);
          output_append (buffer, str, p);
        }
      
      while (*p && *p == '\n')
        {
          output_newline (buffer);
          ++p;
        }

      str = p++;
    }
}

/* Dump the content of BUFFER into FILE.  */

static void
dump_output (buffer, file)
     struct output_buffer *buffer;
     FILE *file;
{
  char *text;
  
  obstack_1grow (&buffer->obstack, '\0');
  text = obstack_finish (&buffer->obstack);
  fputs (text, file);
  obstack_free (&buffer->obstack, text);
  buffer->line_length = 0;
}

static char *
vbuild_message_string (msgid, ap)
     const char *msgid;
     va_list ap;
{
  char *str;

  vasprintf (&str, msgid, ap);
  return str;
}

/*  Return a malloc'd string containing MSGID formatted a la
    printf.  The caller is reponsible for freeing the memory.  */

static char *
build_message_string VPARAMS ((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;
  char *str;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  str = vbuild_message_string (msgid, ap);

  va_end (ap);

  return str;
}


/* Return a malloc'd string describing a location.  The caller is
   responsible for freeing the memory.  */

static char *
build_location_prefix (file, line, warn)
     const char *file;
     int line;
     int warn;
{
  if (file)
    {
      if (warn)
	return build_message_string ("%s:%d: warning: ", file, line);
      else
	return build_message_string ("%s:%d: ", file, line);
    }
  else
    {
      if (warn)
	return build_message_string ("%s: warning: ", progname);
      else
	return build_message_string ("%s: ", progname);
    }
}

/* Format a MESSAGE into BUFFER.  Automatically wrap lines.  */

static void
voutput_notice (buffer, msgid, ap)
     struct output_buffer *buffer;
     const char *msgid;
     va_list ap;
{
  char *message = vbuild_message_string (msgid, ap);

  output_puts (buffer, message);
  free (message);
}


/* Format a message into BUFFER a la printf.  */

static void
output_printf VPARAMS ((struct output_buffer *buffer, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  struct output_buffer *buffer;
  const char *msgid;
#endif
  va_list ap;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  buffer = va_arg (ap, struct output_buffer *);
  msgid = va_arg (ap, const char *);
#endif

  voutput_notice (buffer, msgid, ap);
  va_end (ap);
}


/* Format a MESSAGE into FILE.  Do line wrapping, starting new lines
   with PREFIX.  */

static void
line_wrapper_printf VPARAMS ((FILE *file, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  FILE *file;
  const char *msgid;
#endif
  struct output_buffer buffer;
  va_list ap;
  
  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  file = va_arg (ap, FILE *);
  msgid = va_arg (ap, const char *);
#endif  

  init_output_buffer (&buffer, NULL, output_maximum_width);
  voutput_notice (&buffer, msgid, ap);
  dump_output (&buffer, file);

  va_end (ap);
}


static void
vline_wrapper_message_with_location (file, line, warn, msgid, ap)
     const char *file;
     int line;
     int warn;
     const char *msgid;
     va_list ap;
{
  struct output_buffer buffer;
  
  init_output_buffer
    (&buffer, build_location_prefix (file, line, warn), output_maximum_width);
  voutput_notice (&buffer, msgid, ap);
  dump_output (&buffer, stderr);
  free ((char*)get_output_prefix (&buffer));
  fputc ('\n', stderr);
}


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
notice VPARAMS ((const char *msgid, ...))
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
  struct output_buffer buffer;

  if (doing_line_wrapping ())
    init_output_buffer
      (&buffer,
       build_location_prefix (DECL_SOURCE_FILE (decl),
                              DECL_SOURCE_LINE (decl), warn),
       output_maximum_width);
  else
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
    {
      if (doing_line_wrapping ())
        output_printf (&buffer, "%.*s", (int)(p - _(msgid)), _(msgid));
      else
        fprintf (stderr, "%.*s", (int)(p - _(msgid)), _(msgid));
    }

  if (*p == '%')		/* Print the name.  */
    {
      const char *n = (DECL_NAME (decl)
		 ? (*decl_printable_name) (decl, 2)
		 : "((anonymous))");
      if (doing_line_wrapping ())
        output_puts (&buffer, n);
      else
        fputs (n, stderr);
      while (*p)
	{
	  ++p;
	  if (ISALPHA (*(p - 1) & 0xFF))
	    break;
	}
    }

  if (*p)			/* Print the rest of the message.  */
    {
      if (doing_line_wrapping ())
        voutput_notice (&buffer, p, ap);
      else
        vfprintf (stderr, p, ap);
    }

  if (doing_line_wrapping())
    {
      dump_output (&buffer, stderr);
      free ((char *)get_output_prefix (&buffer));
    }
  
  fputc ('\n', stderr);
}

/* Figure file and line of the given INSN.  */

static void
file_and_line_for_asm (insn, pfile, pline)
     rtx insn;
     const char **pfile;
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
  if (doing_line_wrapping ())
    vline_wrapper_message_with_location (file, line, 0, msgid, ap);
  else
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
  const char *file;
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

static void (*fatal_function) PARAMS ((const char *, va_list));

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
      if (doing_line_wrapping ())
        vline_wrapper_message_with_location (file, line, 1, msgid, ap);
      else
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
      const char *file;
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
fnotice VPARAMS ((FILE *file, const char *msgid, ...))
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
pedwarn VPARAMS ((const char *msgid, ...))
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
pedwarn_with_decl VPARAMS ((tree decl, const char *msgid, ...))
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
pedwarn_with_file_and_line VPARAMS ((const char *file, int line,
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
sorry VPARAMS ((const char *msgid, ...))
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
        {
          if (doing_line_wrapping ())
            line_wrapper_printf
              (stderr, " %s", (*decl_printable_name) (decl, 2));
          else
            fprintf (stderr, " %s", (*decl_printable_name) (decl, 2));
        }
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
      char *prefix = NULL;
      struct output_buffer buffer;
      
      if (file)
        prefix = build_message_string ("%s: ", file);

      if (doing_line_wrapping ())
        init_output_buffer (&buffer, prefix, output_maximum_width);
      else
        {
          if (file)
            fprintf (stderr, "%s: ", file);
        }
      
      if (current_function_decl == NULL)
        {
          if (doing_line_wrapping ())
            output_printf (&buffer, "At top level:\n");
          else
            notice ("At top level:\n");
        }
      else
	{
	  if (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
            {
              if (doing_line_wrapping ())
                output_printf
                  (&buffer, "In method `%s':\n",
                   (*decl_printable_name) (current_function_decl, 2));
              else
                notice ("In method `%s':\n",
                        (*decl_printable_name) (current_function_decl, 2));
            }
	  else
            {
              if (doing_line_wrapping ())
                output_printf
                  (&buffer, "In function `%s':\n",
                   (*decl_printable_name) (current_function_decl, 2));
              else
                notice ("In function `%s':\n",
                        (*decl_printable_name) (current_function_decl, 2));
            }
	}

      last_error_function = current_function_decl;

      if (doing_line_wrapping ())
        dump_output (&buffer, stderr);
      
      free (prefix);
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
error_with_file_and_line VPARAMS ((const char *file, int line,
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
error_with_decl VPARAMS ((tree decl, const char *msgid, ...))
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
error_for_asm VPARAMS ((rtx insn, const char *msgid, ...))
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
error VPARAMS ((const char *msgid, ...))
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
     void (*f) PARAMS ((const char *, va_list));
{
  fatal_function = f;
}

void
fatal VPARAMS ((const char *msgid, ...))
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
warning_with_file_and_line VPARAMS ((const char *file, int line,
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
warning_with_decl VPARAMS ((tree decl, const char *msgid, ...))
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
warning_for_asm VPARAMS ((rtx insn, const char *msgid, ...))
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
warning VPARAMS ((const char *msgid, ...))
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

