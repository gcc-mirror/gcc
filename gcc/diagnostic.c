/* Language-independent diagnostic subroutines for the GNU C compiler
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

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
#include "diagnostic.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free

#define output_formatted_integer(BUFFER, FORMAT, INTEGER) \
  do {                                                    \
    sprintf (digit_buffer, FORMAT, INTEGER);              \
    output_add_string (BUFFER, digit_buffer);             \
  } while (0)

#define output_text_length(BUFFER) (BUFFER)->line_length
#define is_starting_newline(BUFFER) (output_text_length (BUFFER) == 0)
#define output_prefix(BUFFER) (BUFFER)->state.prefix
#define line_wrap_cutoff(BUFFER) (BUFFER)->state.maximum_length
#define ideal_line_wrap_cutoff(BUFFER) (BUFFER)->state.ideal_maximum_length
#define prefix_was_emitted_for(BUFFER) (BUFFER)->state.emitted_prefix_p
#define prefixing_policy(BUFFER) (BUFFER)->state.prefixing_rule
#define output_buffer_ptr_to_format_args(BUFFER) (BUFFER)->state.format_args

#define diagnostic_args output_buffer_ptr_to_format_args (diagnostic_buffer)
#define diagnostic_msg output_buffer_text_cursor (diagnostic_buffer)

/* Prototypes. */
static void finish_diagnostic PARAMS ((void));
static void output_do_verbatim PARAMS ((output_buffer *,
                                        const char *, va_list *));
static void output_to_stream PARAMS ((output_buffer *, FILE *));
static void output_format PARAMS ((output_buffer *));

static char *vbuild_message_string PARAMS ((const char *, va_list));
static char *build_message_string PARAMS ((const char *, ...))
     ATTRIBUTE_PRINTF_1;
static char *context_as_prefix PARAMS ((const char *, int, int));
static void output_do_printf PARAMS ((output_buffer *, const char *));
static void format_with_decl PARAMS ((output_buffer *, tree));
static void file_and_line_for_asm PARAMS ((rtx, const char **, int *));
static void diagnostic_for_asm PARAMS ((rtx, const char *, va_list *, int));
static void diagnostic_for_decl PARAMS ((tree, const char *, va_list *, int));
static void vnotice PARAMS ((FILE *, const char *, va_list));
static void set_real_maximum_length PARAMS ((output_buffer *));
                                          
static void output_unsigned_decimal PARAMS ((output_buffer *, unsigned int));
static void output_long_decimal PARAMS ((output_buffer *, long int));
static void output_long_unsigned_decimal PARAMS ((output_buffer *,
                                                  long unsigned int));
static void output_octal PARAMS ((output_buffer *, unsigned int));
static void output_long_octal PARAMS ((output_buffer *, unsigned long int));
static void output_hexadecimal PARAMS ((output_buffer *, unsigned int));
static void output_long_hexadecimal PARAMS ((output_buffer *,
                                             unsigned long int));
static void output_append_r PARAMS ((output_buffer *, const char *, int));
static void wrap_text PARAMS ((output_buffer *, const char *, const char *));
static void maybe_wrap_text PARAMS ((output_buffer *, const char *,
                                     const char *));
static void clear_text_info PARAMS ((output_buffer *));
static void clear_diagnostic_info PARAMS ((output_buffer *));

extern int rtl_dump_and_exit;
extern int inhibit_warnings;
extern int warnings_are_errors;
extern int warningcount;
extern int errorcount;

/* Front-end specific tree formatter, if non-NULL.  */
printer_fn lang_printer = NULL;

/* This must be large enough to hold any printed integer or
   floating-point value.  */
static char digit_buffer[128];

/* An output_buffer surrogate for stderr.  */
static output_buffer global_output_buffer;
output_buffer *diagnostic_buffer = &global_output_buffer;

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

int diagnostic_message_length_per_line;

/* Used to control every diagnostic message formatting.  Front-ends should
   call set_message_prefixing_rule to set up their politics.  */
static int current_prefixing_rule;

/* Initialize the diagnostic message outputting machinery.  */

void
initialize_diagnostics ()
{
  /* By default, we don't line-wrap messages.  */
  diagnostic_message_length_per_line = 0;
  set_message_prefixing_rule (DIAGNOSTICS_SHOW_PREFIX_ONCE);
  /* Proceed to actual initialization.  */
  default_initialize_buffer (diagnostic_buffer);
}

void
set_message_prefixing_rule (rule)
     int rule;
{
  current_prefixing_rule = rule;
}

/* Returns true if BUFFER is in line-wrappind mode.  */
int
output_is_line_wrapping (buffer)
     output_buffer *buffer;
{
  return ideal_line_wrap_cutoff (buffer) > 0;
}

/* Return BUFFER's prefix.  */
const char *
output_get_prefix (buffer)
     const output_buffer *buffer;
{
  return output_prefix (buffer);
}

/* Subroutine of output_set_maximum_length.  Set up BUFFER's
   internal maximum characters per line.  */
static void
set_real_maximum_length (buffer)
     output_buffer *buffer;
{
  /* If we're told not to wrap lines then do the obvious thing.  */
  if (! output_is_line_wrapping (buffer))
    line_wrap_cutoff (buffer) = ideal_line_wrap_cutoff (buffer);
  else
    {
      int prefix_length =
        output_prefix (buffer) ? strlen (output_prefix (buffer)) : 0;
      /* If the prefix is ridiculously too long, output at least
         32 characters.  */
      if (ideal_line_wrap_cutoff (buffer) - prefix_length < 32)
        line_wrap_cutoff (buffer) = ideal_line_wrap_cutoff (buffer) + 32;
      else
        line_wrap_cutoff (buffer) = ideal_line_wrap_cutoff (buffer);
    }
}

/* Sets the number of maximum characters per line BUFFER can output
   in line-wrapping mode.  A LENGTH value 0 suppresses line-wrapping.  */
void
output_set_maximum_length (buffer, length)
     output_buffer *buffer;
     int length;
{
 ideal_line_wrap_cutoff (buffer) = length;
  set_real_maximum_length (buffer);
}

/* Sets BUFFER's PREFIX.  */
void
output_set_prefix (buffer, prefix)
     output_buffer *buffer;
     const char *prefix;
{
  output_prefix (buffer) = prefix;
  set_real_maximum_length (buffer);
  prefix_was_emitted_for (buffer) = 0;
}

/* Free BUFFER's prefix, a previously malloc()'d string.  */

void
output_destroy_prefix (buffer)
     output_buffer *buffer;
{
  if (output_prefix (buffer) != NULL)
    {
      free ((char *) output_prefix (buffer));
      output_prefix (buffer) = NULL;
    }
}

/* Zero out any text output so far in BUFFER.  */
static void
clear_text_info (buffer)
     output_buffer *buffer;
{
  obstack_free (&buffer->obstack, obstack_base (&buffer->obstack));
  output_text_length (buffer) = 0;
}

/* Zero out any diagnostic data used so far by BUFFER.  */
static void
clear_diagnostic_info (buffer)
     output_buffer *buffer;
{
  output_buffer_text_cursor (buffer) = NULL;
  output_buffer_ptr_to_format_args (buffer) = NULL;
  prefix_was_emitted_for (buffer) = 0;
}

/* Construct an output BUFFER with PREFIX and of MAXIMUM_LENGTH
   characters per line.  */
void
init_output_buffer (buffer, prefix, maximum_length)
     output_buffer *buffer;
     const char *prefix;
     int maximum_length;
{
  obstack_init (&buffer->obstack);
  ideal_line_wrap_cutoff (buffer) = maximum_length;
  prefixing_policy (buffer) = current_prefixing_rule;
  output_set_prefix (buffer, prefix);
  output_text_length (buffer) = 0;
  clear_diagnostic_info (buffer);  
}

/* Initialize BUFFER with a NULL prefix and current diagnostic message
   length cutoff.  */
void
default_initialize_buffer (buffer)
     output_buffer *buffer;
{
  init_output_buffer (buffer, NULL, diagnostic_message_length_per_line);
}

/* Recompute diagnostic_buffer's attributes to reflect any change
   in diagnostic formatting global options.  */
void
reshape_diagnostic_buffer ()
{
  ideal_line_wrap_cutoff (diagnostic_buffer) =
    diagnostic_message_length_per_line;
  prefixing_policy (diagnostic_buffer) = current_prefixing_rule;
  set_real_maximum_length (diagnostic_buffer);
}

/* Reinitialize BUFFER.  */
void
output_clear (buffer)
     output_buffer *buffer;
{
  clear_text_info (buffer);
  clear_diagnostic_info (buffer);
}

/* Finishes to construct a NULL-terminated character string representing
   the BUFFERed message.  */
const char *
output_finish (buffer)
     output_buffer *buffer;
{
  obstack_1grow (&buffer->obstack, '\0');
  return (const char *) obstack_finish (&buffer->obstack);
}

/* Return the amount of characters BUFFER can accept to
   make a full line.  */
int
output_space_left (buffer)
     const output_buffer *buffer;
{
  return line_wrap_cutoff (buffer) - output_text_length (buffer);
}

/* Write out BUFFER's prefix.  */
void
output_emit_prefix (buffer)
     output_buffer *buffer;
{
  if (output_prefix (buffer) != NULL)
    {
      switch (prefixing_policy (buffer))
        {
        default:
        case DIAGNOSTICS_SHOW_PREFIX_NEVER:
          break;

        case DIAGNOSTICS_SHOW_PREFIX_ONCE:
          if (prefix_was_emitted_for (buffer))
            break;
          /* Else fall through.  */

        case DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE:
          {
            int prefix_length = strlen (output_prefix (buffer));
            output_append_r (buffer, output_prefix (buffer), prefix_length);
            prefix_was_emitted_for (buffer) = 1;
          }
          break;
        }
    }
}

/* Have BUFFER start a new line.  */
void
output_add_newline (buffer)
     output_buffer *buffer;
{
  obstack_1grow (&buffer->obstack, '\n');
  output_text_length (buffer) = 0;
}

/* Appends a character to BUFFER.  */
void
output_add_character (buffer, c)
     output_buffer *buffer;
     int c;
{
  if (output_is_line_wrapping (buffer) && output_space_left (buffer) <= 0)
    output_add_newline (buffer);
  obstack_1grow (&buffer->obstack, c);
  ++output_text_length (buffer);
}

/* Adds a space to BUFFER.  */
void
output_add_space (buffer)
     output_buffer *buffer;
{
  if (output_is_line_wrapping (buffer) && output_space_left (buffer) <= 0)
    {
      output_add_newline (buffer);
      return;
    }
  obstack_1grow (&buffer->obstack, ' ');
  ++output_text_length (buffer);
}

/* These functions format an INTEGER into BUFFER as suggested by their
   names.  */
void
output_decimal (buffer, i)
     output_buffer *buffer;
     int i;
{
  output_formatted_integer (buffer, "%d", i);
}

static void
output_long_decimal (buffer, i)
     output_buffer *buffer;
     long int i;
{
  output_formatted_integer (buffer, "%ld", i);
}

static void
output_unsigned_decimal (buffer, i)
     output_buffer *buffer;
     unsigned int i;
{
  output_formatted_integer (buffer, "%u", i);
}

static void
output_long_unsigned_decimal (buffer, i)
     output_buffer *buffer;
     long unsigned int i;
{
  output_formatted_integer (buffer, "%lu", i);
}

static void
output_octal (buffer, i)
     output_buffer *buffer;
     unsigned int i;
{
  output_formatted_integer (buffer, "%o", i);
}

static void
output_long_octal (buffer, i)
     output_buffer *buffer;
     unsigned long int i;
{
  output_formatted_integer (buffer, "%lo", i);
}

static void
output_hexadecimal (buffer, i)
     output_buffer *buffer;
     unsigned int i;
{
  output_formatted_integer (buffer, "%x", i);
}

static void
output_long_hexadecimal (buffer, i)
     output_buffer *buffer;
     unsigned long int i;
{
  output_formatted_integer (buffer, "%lx", i);
}

/* Append to BUFFER a string specified by its STARTING character
   and LENGTH.  */
static void
output_append_r (buffer, start, length)
     output_buffer *buffer;
     const char *start;
     int length;
{
  obstack_grow (&buffer->obstack, start, length);
  output_text_length (buffer) += length;
}

/* Append a string deliminated by START and END to BUFFER.  No wrapping is
   done.  However, if beginning a new line then emit output_prefix (BUFFER)
   and skip any leading whitespace if appropriate.  The caller must ensure
   that it is safe to do so.  */
void
output_append (buffer, start, end)
     output_buffer *buffer;
     const char *start;
     const char *end;
{
  /* Emit prefix and skip whitespace if we're starting a new line.  */
  if (is_starting_newline (buffer))
    {
      output_emit_prefix (buffer);
      if (output_is_line_wrapping (buffer))
        while (start != end && *start == ' ')
          ++start;
    }
  output_append_r (buffer, start, end - start);
}

/* Wrap a text delimited by START and END into BUFFER.  */
static void
wrap_text (buffer, start, end)
     output_buffer *buffer;
     const char *start;
     const char *end;
{
  while (start != end)
    {
      /* Dump anything bodered by whitespaces.  */ 
      {
        const char *p = start;
        while (p != end && *p != ' ' && *p != '\n')
          ++p;
        if (p - start >= output_space_left (buffer))
          output_add_newline (buffer);
        output_append (buffer, start, p);
        start = p;
      }

      if (start != end && *start == ' ')
        {
          output_add_space (buffer);
          ++start;
        }
      if (start != end && *start == '\n')
        {
          output_add_newline (buffer);
          ++start;
        }
    }
}

/* Same as wrap_text but wrap text only when in line-wrapping mode.  */
static void
maybe_wrap_text (buffer, start, end)
     output_buffer *buffer;
     const char *start;
     const char *end;
{
  if (output_is_line_wrapping (buffer))
    wrap_text (buffer, start, end);
  else
    output_append (buffer, start, end);
}


/* Append a STRING to BUFFER; the STRING maybe be line-wrapped if in
   appropriate mode.  */

void
output_add_string (buffer, str)
     output_buffer *buffer;
     const char *str;
{
  maybe_wrap_text (buffer, str, str + (str ? strlen (str) : 0));
}

/* Flush the content of BUFFER onto FILE and reinitialize BUFFER.  */

static void
output_to_stream (buffer, file)
     output_buffer *buffer;
     FILE *file;
{
  const char *text = output_finish (buffer);
  fputs (text, file);
  clear_text_info (buffer);
}

/* Format a message pointed to by output_buffer_text_cursor (BUFFER) using
   output_buffer_format_args (BUFFER) as appropriate.  The following format
   specifiers are recognized as  being language independent:
   %d, %i: (signed) integer in base ten.
   %u: unsigned integer in base ten.
   %o: unsigned integer in base eight.
   %x: unsigned integer in base sixteen.
   %ld, %li, %lo, %lu, %lx: long versions of the above.
   %c: character.
   %s: string.
   %%: `%'.
   %*.s: a substring the length of which is specified by an integer.  */
static void
output_format (buffer)
     output_buffer *buffer;
{
  for (; *output_buffer_text_cursor (buffer);
       ++output_buffer_text_cursor (buffer))
    {
      int long_integer = 0;
      /* Ignore text.  */
      {
        const char *p = output_buffer_text_cursor (buffer);
        while (*p && *p != '%')
          ++p;
        maybe_wrap_text (buffer, output_buffer_text_cursor (buffer), p);
        output_buffer_text_cursor (buffer) = p;
      }
      if (!*output_buffer_text_cursor (buffer))
        break;

      /* We got a '%'.  Let's see what happens. Record whether we're
         parsing a long integer format specifier.  */
      if (*++output_buffer_text_cursor (buffer) == 'l')
        {
          long_integer = 1;
          ++output_buffer_text_cursor (buffer);
        }

      /* Handle %c, %d, %i, %ld, %li, %lo, %lu, %lx, %o, %s, %u,
         %x, %.*s; %%.  And nothing else.  Front-ends should install
         printers to grok language specific format specifiers.  */
      switch (*output_buffer_text_cursor (buffer))
        {
        case 'c':
          output_add_character
            (buffer, va_arg (output_buffer_format_args (buffer), int));
          break;
          
        case 'd':
        case 'i':
          if (long_integer)
            output_long_decimal
              (buffer, va_arg (output_buffer_format_args (buffer), long int));
          else
            output_decimal
              (buffer, va_arg (output_buffer_format_args (buffer), int));
          break;

        case 'o':
          if (long_integer)
            output_long_octal (buffer,
                               va_arg (output_buffer_format_args (buffer),
                                       unsigned long int));
          else
            output_octal (buffer,
                          va_arg (output_buffer_format_args (buffer),
                                  unsigned int));
          break;

        case 's':
          output_add_string (buffer,
                             va_arg (output_buffer_format_args (buffer),
                                     const char *));
          break;

        case 'u':
          if (long_integer)
            output_long_unsigned_decimal
              (buffer, va_arg (output_buffer_format_args (buffer),
                               long unsigned int));
          else
            output_unsigned_decimal
              (buffer, va_arg (output_buffer_format_args (buffer),
                               unsigned int));
          
        case 'x':
          if (long_integer)
            output_long_hexadecimal
              (buffer, va_arg (output_buffer_format_args (buffer),
                               unsigned long int));
          else
            output_hexadecimal
              (buffer, va_arg (output_buffer_format_args (buffer),
                               unsigned int));
          break;

        case '%':
          output_add_character (buffer, '%');
          break;

        case '.':
          {
            int n;
            const char *s;
            /* We handle no precision specifier but `%.*s'.  */
            if (*++output_buffer_text_cursor (buffer) != '*')
              abort ();
            else if (*++output_buffer_text_cursor (buffer) != 's')
              abort();
            n = va_arg (output_buffer_format_args (buffer), int);
            s = va_arg (output_buffer_format_args (buffer), const char *);
            output_append (buffer, s, s + n);
          }
          break;

        default:
          if (!lang_printer || !(*lang_printer) (buffer))
            {
              /* Hmmm.  The front-end failed to install a format translator
                 but called us with an unrecognized format.  Sorry.  */
              abort();
            }
        }
    }
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
context_as_prefix (file, line, warn)
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
output_do_printf (buffer, msgid)
     output_buffer *buffer;
     const char *msgid;
{
  char *message = vbuild_message_string (msgid,
                                         output_buffer_format_args (buffer));

  output_add_string (buffer, message);
  free (message);
}


/* Format a message into BUFFER a la printf.  */

void
output_printf VPARAMS ((struct output_buffer *buffer, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  struct output_buffer *buffer;
  const char *msgid;
#endif
  va_list ap;
  va_list *old_args;

  VA_START (ap, msgid);
#ifndef ANSI_PROTOTYPES
  buffer = va_arg (ap, output_buffer *);
  msgid = va_arg (ap, const char *);
#endif
  old_args = output_buffer_ptr_to_format_args (buffer);
  output_buffer_ptr_to_format_args (buffer) = &ap;
  output_do_printf (buffer, msgid);
  output_buffer_ptr_to_format_args (buffer) = old_args;
  va_end (ap);
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

/* Print a message relevant to the given DECL.  */

static void
format_with_decl (buffer, decl)
     output_buffer *buffer;
     tree decl;
{
  const char *p;
  
  /* Do magic to get around lack of varargs support for insertion
     of arguments into existing list.  We know that the decl is first;
     we ass_u_me that it will be printed with "%s".  */
  for (p = output_buffer_text_cursor (buffer); *p; ++p)
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

  /* Print the left-hand substring.  */
  maybe_wrap_text (buffer, output_buffer_text_cursor (buffer), p);
  
  if (*p == '%')		/* Print the name.  */
    {
      const char *n = (DECL_NAME (decl)
		 ? (*decl_printable_name) (decl, 2)
		 : _("((anonymous))"));
      output_add_string (buffer, n);
      while (*p)
	{
	  ++p;
	  if (ISALPHA (*(p - 1) & 0xFF))
	    break;
	}
    }

  if (*p)			/* Print the rest of the message.  */
    {
      output_buffer_text_cursor (buffer) = p;
      output_format (buffer);
    }
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

/* Report a diagnostic MESSAGE (an errror or a WARNING) at the line number
   of the insn INSN.  This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */
static void
diagnostic_for_asm (insn, msg, args_ptr, warn)
     rtx insn;
     const char *msg;
     va_list *args_ptr;
     int warn;
{
  const char *file;
  int line;

  file_and_line_for_asm (insn, &file, &line);
  report_diagnostic (msg, args_ptr, file, line, warn);
}

/* Report a diagnostic MESSAGE at the declaration DECL.
   MSG is a format string which uses %s to substitute the declaration
   name; subsequent substitutions are a la output_format.  */
static void
diagnostic_for_decl (decl, msg, args_ptr, warn)
     tree decl;
     const char *msg;
     va_list *args_ptr;
     int warn;
{
  output_state os;

  if (!count_error (warn))
    return;
  os = diagnostic_buffer->state;
  report_error_function (DECL_SOURCE_FILE (decl));
  output_set_prefix
    (diagnostic_buffer, context_as_prefix
     (DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl), warn));
  output_buffer_ptr_to_format_args (diagnostic_buffer) = args_ptr;
  output_buffer_text_cursor (diagnostic_buffer) = msg;
  format_with_decl (diagnostic_buffer, decl);
  finish_diagnostic ();
  output_destroy_prefix (diagnostic_buffer);
  
  diagnostic_buffer->state = os;
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
	  verbatim ("%s: warnings being treated as errors\n", progname);
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
  verbatim ("%s: %s: I/O error\n", progname, name);
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

  report_diagnostic (msgid, &ap, input_filename, lineno,
                     !flag_pedantic_errors);
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
  /* We don't want -pedantic-errors to cause the compilation to fail from
     "errors" in system header files.  Sometimes fixincludes can't fix what's
     broken (eg: unsigned char bitfields - fixing it may change the alignment
     which will cause programs to mysteriously fail because the C library
     or kernel uses the original layout).  There's no point in issuing a
     warning either, it's just unnecessary noise.  */
  if (!DECL_IN_SYSTEM_HEADER (decl))
    diagnostic_for_decl (decl, msgid, &ap, !flag_pedantic_errors);
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

  report_diagnostic (msgid, &ap, file, line, !flag_pedantic_errors);
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
  output_state os;

  os = diagnostic_buffer->state;
  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif
  ++sorrycount;
  output_set_prefix
    (diagnostic_buffer, context_as_prefix (input_filename, lineno, 0));
  output_printf (diagnostic_buffer, "sorry, not implemented: ");
  output_buffer_ptr_to_format_args (diagnostic_buffer) = &ap;
  output_buffer_text_cursor (diagnostic_buffer) = msgid;
  output_format (diagnostic_buffer);
  finish_diagnostic ();
  diagnostic_buffer->state = os;
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
	verbatim ("%s ", IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
        verbatim (" %s", (*decl_printable_name) (decl, 2));
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
      char *prefix = file ? build_message_string ("%s: ", file) : NULL;
      output_state os;

      os = diagnostic_buffer->state;
      output_set_prefix (diagnostic_buffer, prefix);
      
      if (current_function_decl == NULL)
        {
          output_add_string (diagnostic_buffer, "At top level:");
          output_add_newline (diagnostic_buffer);
        }
      else
	{
	  if (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
            output_printf
              (diagnostic_buffer, "In method `%s':\n",
               (*decl_printable_name) (current_function_decl, 2));
	  else
            output_printf
              (diagnostic_buffer, "In function `%s':\n",
               (*decl_printable_name) (current_function_decl, 2));
	}

      last_error_function = current_function_decl;
      output_to_stream (diagnostic_buffer, stderr);
      diagnostic_buffer->state = os;
      free ((char*) prefix);
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
      verbatim ("\n");
      need_error_newline = 0;
    }

  if (input_file_stack && input_file_stack->next != 0
      && input_file_stack_tick != last_error_tick)
    {
      for (p = input_file_stack->next; p; p = p->next)
	if (p == input_file_stack->next)
	  verbatim ("In file included from %s:%d", p->name, p->line);
	else
	  verbatim (",\n                 from %s:%d", p->name, p->line);
      verbatim (":\n");
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

  report_diagnostic (msgid, &ap, file, line, /* warn = */ 0);
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

  diagnostic_for_decl (decl, msgid, &ap, /* warn = */ 0);
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

  diagnostic_for_asm (insn, msgid, &ap, /* warn = */ 0);
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

  report_diagnostic (msgid, &ap, input_filename, lineno, /* warn = */ 0);
  va_end (ap);
}

/* Set the function to call when a fatal error occurs.  */

static void (*fatal_function) PARAMS ((const char *, va_list));

void
set_fatal_function (f)
     void (*f) PARAMS ((const char *, va_list));
{
  fatal_function = f;
}

/* Report a fatal error at the current line number.  Allow a front end to
   intercept the message.  */
void
fatal VPARAMS ((const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;
  va_list args_for_fatal_msg;

  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif
  va_copy (args_for_fatal_msg, ap);

  if (fatal_function != NULL)
    (*fatal_function) (_(msgid), args_for_fatal_msg);
  va_end (args_for_fatal_msg);
  report_diagnostic (msgid, &ap, input_filename, lineno, 0);
  va_end (ap);
  exit (FATAL_EXIT_CODE);
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

  report_diagnostic (msgid, &ap, file, line, /* warn = */ 1);
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

  diagnostic_for_decl (decl, msgid, &ap, /* warn = */ 1);
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

  diagnostic_for_asm (insn, msgid, &ap, /* warn = */ 1);
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

  report_diagnostic (msgid, &ap, input_filename, lineno, /* warn = */ 1);
  va_end (ap);
}

/* Flush diagnostic_buffer content on stderr.  */
static void
finish_diagnostic ()
{
  output_to_stream (diagnostic_buffer, stderr);
  clear_diagnostic_info (diagnostic_buffer);
  fputc ('\n', stderr);
  fflush (stderr);
}

/* Helper subroutine of output_verbatim and verbatim. Do the approriate
   settings needed by BUFFER for a verbatim formatting.  */
static void
output_do_verbatim (buffer, msg, args_ptr)
     output_buffer *buffer;
     const char *msg;
     va_list *args_ptr;
{
  output_state os;

  os = buffer->state;
  output_prefix (buffer) = NULL;
  prefixing_policy (buffer) = DIAGNOSTICS_SHOW_PREFIX_NEVER;
  output_buffer_text_cursor (buffer) = msg;
  output_buffer_ptr_to_format_args (buffer) = args_ptr;
  output_set_maximum_length (buffer, 0);
  output_format (buffer);
  buffer->state = os;
}

/* Output MESSAGE verbatim into BUFFER.  */
void
output_verbatim VPARAMS ((output_buffer *buffer, const char *msg, ...))
{
#ifndef ANSI_PROTOTYPES
  output_buffer *buffer;
  const char *msg;
#endif
  va_list ap;

  VA_START (ap, msg);
#ifndef ANSI_PROTOTYPES
  buffer = va_arg (ap, output_buffer *);
  msg = va_arg (ap, const char *);
#endif
  output_do_verbatim (buffer, msg, &ap);
  va_end (ap);
}

/* Same as above but use diagnostic_buffer.  */
void
verbatim VPARAMS ((const char *msg, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *msg;
#endif
  va_list ap;

  VA_START (ap, msg);
#ifndef ANSI_PROTOTYPES
  msg = va_arg (ap, const char *);
#endif
  output_do_verbatim (diagnostic_buffer, msg, &ap);
  output_to_stream (diagnostic_buffer, stderr);
  va_end (ap);
}

/* Report a diagnostic MESSAGE (an error or a WARNING) involving
   entities in ARGUMENTS.  FILE and LINE indicate where the diagnostic
   occurs.  This function is *the* subroutine in terms of which front-ends
   should implement their specific diagnostic handling modules.
   The front-end independent format specifiers are exactly those described
   in the documentation of output_format.  */
void
report_diagnostic (msg, args_ptr, file, line, warn)
     const char *msg;
     va_list *args_ptr;
     const char *file;
     int line;
     int warn;
{
  output_state os;

  if (!count_error (warn))
    return;
  os = diagnostic_buffer->state;
  diagnostic_msg = msg;
  diagnostic_args = args_ptr;
  report_error_function (file);
  output_set_prefix
    (diagnostic_buffer, context_as_prefix (file, line, warn));
  output_format (diagnostic_buffer);
  finish_diagnostic ();
  output_destroy_prefix (diagnostic_buffer);
  diagnostic_buffer->state = os;
}
