/* Language-independent diagnostic subroutines for the GNU Compiler Collection
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


/* This file implements the language independent aspect of diagnostic
   message module.  */

#include "config.h"
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "input.h"
#include "toplev.h"
#include "intl.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"

#define output_text_length(BUFFER) (BUFFER)->line_length
#define is_starting_newline(BUFFER) (output_text_length (BUFFER) == 0)
#define line_wrap_cutoff(BUFFER) (BUFFER)->state.maximum_length
#define prefix_was_emitted_for(BUFFER) (BUFFER)->state.emitted_prefix_p

/* Prototypes.  */
static void output_flush PARAMS ((output_buffer *));
static void output_do_verbatim PARAMS ((output_buffer *, text_info *));
static void output_buffer_to_stream PARAMS ((output_buffer *));
static void output_format PARAMS ((output_buffer *, text_info *));
static void output_indent PARAMS ((output_buffer *));

static char *vbuild_message_string PARAMS ((const char *, va_list))
     ATTRIBUTE_PRINTF (1, 0);
static char *build_message_string PARAMS ((const char *, ...))
     ATTRIBUTE_PRINTF_1;
static void format_with_decl PARAMS ((output_buffer *, text_info *, tree));
static void diagnostic_for_decl PARAMS ((diagnostic_info *, tree));
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
static void output_pointer PARAMS ((output_buffer *, void *));
static void output_append_r PARAMS ((output_buffer *, const char *, int));
static void wrap_text PARAMS ((output_buffer *, const char *, const char *));
static void maybe_wrap_text PARAMS ((output_buffer *, const char *,
				     const char *));
static void output_clear_data PARAMS ((output_buffer *));

static void default_diagnostic_starter PARAMS ((diagnostic_context *,
                                                diagnostic_info *));
static void default_diagnostic_finalizer PARAMS ((diagnostic_context *,
                                                  diagnostic_info *));

static void error_recursion PARAMS ((diagnostic_context *)) ATTRIBUTE_NORETURN;
static bool text_specifies_location PARAMS ((text_info *, location_t *));

extern int rtl_dump_and_exit;
extern int warnings_are_errors;

/* A diagnostic_context surrogate for stderr.  */
static diagnostic_context global_diagnostic_context;
diagnostic_context *global_dc = &global_diagnostic_context;


/* Subroutine of output_set_maximum_length.  Set up BUFFER's
   internal maximum characters per line.  */
static void
set_real_maximum_length (buffer)
     output_buffer *buffer;
{
  /* If we're told not to wrap lines then do the obvious thing.  In case
   we'll emit prefix only once per diagnostic message, it is appropriate
  not to increase unnecessarily the line-length cut-off.  */
  if (!output_is_line_wrapping (buffer)
      || output_prefixing_rule (buffer) == DIAGNOSTICS_SHOW_PREFIX_ONCE
      || output_prefixing_rule (buffer) == DIAGNOSTICS_SHOW_PREFIX_NEVER)
    line_wrap_cutoff (buffer) = output_line_cutoff (buffer);
  else
    {
      int prefix_length = buffer->state.prefix ?
        strlen (buffer->state.prefix) : 0;
      /* If the prefix is ridiculously too long, output at least
         32 characters.  */
      if (output_line_cutoff (buffer) - prefix_length < 32)
	line_wrap_cutoff (buffer) = output_line_cutoff (buffer) + 32;
      else
	line_wrap_cutoff (buffer) = output_line_cutoff (buffer);
    }
}

/* Sets the number of maximum characters per line BUFFER can output
   in line-wrapping mode.  A LENGTH value 0 suppresses line-wrapping.  */
void
output_set_maximum_length (buffer, length)
     output_buffer *buffer;
     int length;
{
  output_line_cutoff (buffer) = length;
  set_real_maximum_length (buffer);
}

/* Sets BUFFER's PREFIX.  */
void
output_set_prefix (buffer, prefix)
     output_buffer *buffer;
     const char *prefix;
{
  buffer->state.prefix = prefix;
  set_real_maximum_length (buffer);
  prefix_was_emitted_for (buffer) = false;
  output_indentation (buffer) = 0;
}

/*  Return a pointer to the last character emitted in the output
    BUFFER area.  A NULL pointer means no character available.  */
const char *
output_last_position (buffer)
     const output_buffer *buffer;
{
  const char *p = NULL;

  if (obstack_base (&buffer->obstack) != obstack_next_free (&buffer->obstack))
    p = ((const char *) obstack_next_free (&buffer->obstack)) - 1;
  return p;
}

/* Free BUFFER's prefix, a previously malloc'd string.  */
void
output_destroy_prefix (buffer)
     output_buffer *buffer;
{
  if (buffer->state.prefix != NULL)
    {
      free ((char *) buffer->state.prefix);
      buffer->state.prefix = NULL;
    }
}

/* Zero out any text output so far in BUFFER.  */
void
output_clear_message_text (buffer)
     output_buffer *buffer;
{
  obstack_free (&buffer->obstack, obstack_base (&buffer->obstack));
  output_text_length (buffer) = 0;
}

/* Zero out any formatting data used so far by BUFFER.  */
static void
output_clear_data (buffer)
     output_buffer *buffer;
{
  prefix_was_emitted_for (buffer) = false;
  output_indentation (buffer) = 0;
}

/* Construct an output BUFFER with PREFIX and of MAXIMUM_LENGTH
   characters per line.  */
void
init_output_buffer (buffer, prefix, maximum_length)
     output_buffer *buffer;
     const char *prefix;
     int maximum_length;
{
  memset (buffer, 0, sizeof (output_buffer));
  obstack_init (&buffer->obstack);
  output_buffer_attached_stream (buffer) = stderr;
  output_line_cutoff (buffer) = maximum_length;
  output_prefixing_rule (buffer) = diagnostic_prefixing_rule (global_dc);
  output_set_prefix (buffer, prefix);
  output_text_length (buffer) = 0;
  output_clear_data (buffer);
}

/* Reinitialize BUFFER.  */
void
output_clear (buffer)
     output_buffer *buffer;
{
  output_clear_message_text (buffer);
  output_clear_data (buffer);
}

/* Finishes constructing a NULL-terminated character string representing
   the BUFFERed message.  */
const char *
output_finalize_message (buffer)
     output_buffer *buffer;
{
  obstack_1grow (&buffer->obstack, '\0');
  return output_message_text (buffer);
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
  if (buffer->state.prefix != NULL)
    {
      switch (output_prefixing_rule (buffer))
	{
	default:
	case DIAGNOSTICS_SHOW_PREFIX_NEVER:
	  break;

	case DIAGNOSTICS_SHOW_PREFIX_ONCE:
	  if (prefix_was_emitted_for (buffer))
	    {
	      output_indent (buffer);
	      break;
	    }
	  output_indentation (buffer) += 3;
	  /* Fall through.  */

	case DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE:
	  {
	    int prefix_length = strlen (buffer->state.prefix);
	    output_append_r (buffer, buffer->state.prefix, prefix_length);
	    prefix_was_emitted_for (buffer) = true;
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
  output_formatted_scalar (buffer, "%d", i);
}

static void
output_long_decimal (buffer, i)
     output_buffer *buffer;
     long int i;
{
  output_formatted_scalar (buffer, "%ld", i);
}

static void
output_unsigned_decimal (buffer, i)
     output_buffer *buffer;
     unsigned int i;
{
  output_formatted_scalar (buffer, "%u", i);
}

static void
output_long_unsigned_decimal (buffer, i)
     output_buffer *buffer;
     long unsigned int i;
{
  output_formatted_scalar (buffer, "%lu", i);
}

static void
output_octal (buffer, i)
     output_buffer *buffer;
     unsigned int i;
{
  output_formatted_scalar (buffer, "%o", i);
}

static void
output_long_octal (buffer, i)
     output_buffer *buffer;
     unsigned long int i;
{
  output_formatted_scalar (buffer, "%lo", i);
}

static void
output_hexadecimal (buffer, i)
     output_buffer *buffer;
     unsigned int i;
{
  output_formatted_scalar (buffer, "%x", i);
}

static void
output_long_hexadecimal (buffer, i)
     output_buffer *buffer;
     unsigned long int i;
{
  output_formatted_scalar (buffer, "%lx", i);
}

static void
output_pointer (buffer, p)
     output_buffer *buffer;
     void *p;
{
  output_formatted_scalar (buffer, HOST_PTR_PRINTF, p);
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
   done.  However, if beginning a new line then emit BUFFER->state.prefix
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

static void
output_indent (buffer)
     output_buffer *buffer;
{
  int n = output_indentation (buffer);
  int i;

  for (i = 0; i < n; ++i)
    output_add_character (buffer, ' ');
}

/* Wrap a text delimited by START and END into BUFFER.  */
static void
wrap_text (buffer, start, end)
     output_buffer *buffer;
     const char *start;
     const char *end;
{
  bool is_wrapping = output_is_line_wrapping (buffer);

  while (start != end)
    {
      /* Dump anything bordered by whitespaces.  */
      {
	const char *p = start;
	while (p != end && *p != ' ' && *p != '\n')
	  ++p;
	if (is_wrapping && p - start >= output_space_left (buffer))
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


/* Append a STRING to BUFFER; the STRING might be line-wrapped if in
   appropriate mode.  */
void
output_add_string (buffer, str)
     output_buffer *buffer;
     const char *str;
{
  maybe_wrap_text (buffer, str, str + (str ? strlen (str) : 0));
}

/* Append an identifier ID to BUFFER.  */
void
output_add_identifier (buffer, id)
     output_buffer *buffer;
     tree id;
{
  output_append (buffer, IDENTIFIER_POINTER (id),
		 IDENTIFIER_POINTER (id) + IDENTIFIER_LENGTH (id));
}

/* Flush the content of BUFFER onto the attached stream,
   and reinitialize.  */

static void
output_buffer_to_stream (buffer)
     output_buffer *buffer;
{
  const char *text = output_finalize_message (buffer);
  fputs (text, output_buffer_attached_stream (buffer));
  output_clear_message_text (buffer);
}

/* Format a message pointed to by TEXT.  The following format specifiers are
   recognized as being language independent:
   %d, %i: (signed) integer in base ten.
   %u: unsigned integer in base ten.
   %o: unsigned integer in base eight.
   %x: unsigned integer in base sixteen.
   %ld, %li, %lo, %lu, %lx: long versions of the above.
   %c: character.
   %s: string.
   %p: pointer.
   %%: `%'.
   %*.s: a substring the length of which is specified by an integer.
   %H: location_t.  */
static void
output_format (buffer, text)
     output_buffer *buffer;
     text_info *text;
{
  for (; *text->format_spec; ++text->format_spec)
    {
      bool long_integer = 0;

      /* Ignore text.  */
      {
	const char *p = text->format_spec;
	while (*p && *p != '%')
	  ++p;
	wrap_text (buffer, text->format_spec, p);
        text->format_spec = p;
      }

      if (*text->format_spec == '\0')
	break;

      /* We got a '%'.  Let's see what happens. Record whether we're
         parsing a long integer format specifier.  */
      if (*++text->format_spec == 'l')
	{
	  long_integer = true;
	  ++text->format_spec;
	}

      /* Handle %c, %d, %i, %ld, %li, %lo, %lu, %lx, %o, %s, %u,
         %x, %.*s; %%.  And nothing else.  Front-ends should install
         printers to grok language specific format specifiers.  */
      switch (*text->format_spec)
	{
	case 'c':
	  output_add_character (buffer, va_arg (*text->args_ptr, int));
	  break;

	case 'd':
	case 'i':
	  if (long_integer)
	    output_long_decimal (buffer, va_arg (*text->args_ptr, long int));
	  else
	    output_decimal (buffer, va_arg (*text->args_ptr, int));
	  break;

	case 'o':
	  if (long_integer)
	    output_long_octal (buffer,
			       va_arg (*text->args_ptr, unsigned long int));
	  else
	    output_octal (buffer, va_arg (*text->args_ptr, unsigned int));
	  break;

	case 's':
	  output_add_string (buffer, va_arg (*text->args_ptr, const char *));
	  break;

        case 'p':
          output_pointer (buffer, va_arg (*text->args_ptr, void *));
          break;

	case 'u':
	  if (long_integer)
	    output_long_unsigned_decimal
	      (buffer, va_arg (*text->args_ptr, long unsigned int));
	  else
	    output_unsigned_decimal
	      (buffer, va_arg (*text->args_ptr, unsigned int));
	  break;

	case 'x':
	  if (long_integer)
	    output_long_hexadecimal
	      (buffer, va_arg (*text->args_ptr, unsigned long int));
	  else
	    output_hexadecimal
              (buffer, va_arg (*text->args_ptr, unsigned int));
	  break;

	case '%':
	  output_add_character (buffer, '%');
	  break;

        case 'H':
          {
            const location_t *locus = va_arg (*text->args_ptr, location_t *);
            output_add_string (buffer, "file '");
            output_add_string (buffer, locus->file);
            output_add_string (buffer, "', line ");
            output_decimal (buffer, locus->line);
          }
          break;

	case '.':
	  {
	    int n;
	    const char *s;
	    /* We handle no precision specifier but `%.*s'.  */
	    if (*++text->format_spec != '*')
	      abort ();
	    else if (*++text->format_spec != 's')
	      abort ();
	    n = va_arg (*text->args_ptr, int);
	    s = va_arg (*text->args_ptr, const char *);
	    output_append (buffer, s, s + n);
	  }
	  break;

	default:
	  if (!buffer->format_decoder
              || !(*buffer->format_decoder) (buffer, text))
	    {
	      /* Hmmm.  The front-end failed to install a format translator
                 but called us with an unrecognized format.  Sorry.  */
	      abort ();
	    }
	}
    }
}

static char *
vbuild_message_string (msg, ap)
     const char *msg;
     va_list ap;
{
  char *str;

  vasprintf (&str, msg, ap);
  return str;
}

/*  Return a malloc'd string containing MSG formatted a la
    printf.  The caller is responsible for freeing the memory.  */
static char *
build_message_string VPARAMS ((const char *msg, ...))
{
  char *str;

  VA_OPEN (ap, msg);
  VA_FIXEDARG (ap, const char *, msg);

  str = vbuild_message_string (msg, ap);

  VA_CLOSE (ap);

  return str;
}

/* Same as diagnsotic_build_prefix, but only the source FILE is given.  */
char *
file_name_as_prefix (f)
     const char *f;
{
  return build_message_string ("%s: ", f);
}

/* Format a message into BUFFER a la printf.  */
void
output_printf VPARAMS ((struct output_buffer *buffer, const char *msgid, ...))
{
  text_info text;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, output_buffer *, buffer);
  VA_FIXEDARG (ap, const char *, msgid);

  text.args_ptr = &ap;
  text.format_spec = _(msgid);
  output_format (buffer, &text);
  VA_CLOSE (ap);
}

/* Print a message relevant to the given DECL.  */
static void
format_with_decl (buffer, text, decl)
     output_buffer *buffer;
     text_info *text;
     tree decl;
{
  const char *p;

  /* Do magic to get around lack of varargs support for insertion
     of arguments into existing list.  We know that the decl is first;
     we ass_u_me that it will be printed with "%s".  */
  for (p = text->format_spec; *p; ++p)
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
  maybe_wrap_text (buffer, text->format_spec, p);

  if (*p == '%')		/* Print the name.  */
    {
      const char *const n = (DECL_NAME (decl)
			     ? (*lang_hooks.decl_printable_name) (decl, 2)
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
      text->format_spec = p;
      output_format (buffer, text);
    }
}

/* Flush the content of BUFFER onto the attached stream.  */
static void
output_flush (buffer)
     output_buffer *buffer;
{
  output_buffer_to_stream (buffer);
  output_clear_data (buffer);
  fputc ('\n', output_buffer_attached_stream (buffer));
  fflush (output_buffer_attached_stream (buffer));
}

/* Helper subroutine of output_verbatim and verbatim. Do the appropriate
   settings needed by BUFFER for a verbatim formatting.  */
static void
output_do_verbatim (buffer, text)
     output_buffer *buffer;
     text_info *text;
{
  diagnostic_prefixing_rule_t rule = output_prefixing_rule (buffer);
  int line_cutoff = output_line_cutoff (buffer);

  /* Set verbatim mode.  */
  output_prefixing_rule (buffer) = DIAGNOSTICS_SHOW_PREFIX_NEVER;
  output_line_cutoff (buffer) = 0;
  /* Do the actual formatting.  */
  output_format (buffer, text);
  /* Restore previous settings.  */
  output_prefixing_rule (buffer) = rule;
  output_line_cutoff (buffer) = line_cutoff;
}

/* Output MESSAGE verbatim into BUFFER.  */
void
output_verbatim VPARAMS ((output_buffer *buffer, const char *msgid, ...))
{
  text_info text;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, output_buffer *, buffer);
  VA_FIXEDARG (ap, const char *, msgid);

  text.format_spec = msgid;
  text.args_ptr = &ap;
  output_do_verbatim (buffer, &text);
  VA_CLOSE (ap);
}


/* Initialize the diagnostic message outputting machinery.  */
void
diagnostic_initialize (context)
     diagnostic_context *context;
{
  memset (context, 0, sizeof *context);
  obstack_init (&context->buffer.obstack);

  /* By default, diagnostics are sent to stderr.  */
  output_buffer_attached_stream (&context->buffer) = stderr;

  /* By default, we emit prefixes once per message.  */
  diagnostic_prefixing_rule (context) = DIAGNOSTICS_SHOW_PREFIX_ONCE;

  diagnostic_starter (context) = default_diagnostic_starter;
  diagnostic_finalizer (context) = default_diagnostic_finalizer;
  context->warnings_are_errors_message = warnings_are_errors;
}

/* Returns true if the next format specifier in TEXT is a format specifier
   for a location_t.  If so, update the object pointed by LOCUS to reflect
   the specified location in *TEXT->args_ptr.  */
static bool
text_specifies_location (text, locus)
     text_info *text;
     location_t *locus;
{
  const char *p;
  /* Skip any leading text.  */
  for (p = text->format_spec; *p && *p != '%'; ++p)
    ;

  /* Extract the location information if any.  */
  if (*p == '%' && *++p == 'H')
    {
      *locus = *va_arg (*text->args_ptr, location_t *);
      text->format_spec = p + 1;
      return true;
    }

  return false;
}

void
diagnostic_set_info (diagnostic, msgid, args, file, line, kind)
     diagnostic_info *diagnostic;
     const char *msgid;
     va_list *args;
     const char *file;
     int line;
     diagnostic_t kind;
{
  diagnostic->message.format_spec = msgid;
  diagnostic->message.args_ptr = args;
  /* If the diagnostic message doesn't specify a loccation,
     use FILE and LINE.  */
  if (!text_specifies_location (&diagnostic->message, &diagnostic->location))
    {
      diagnostic->location.file = file;
      diagnostic->location.line = line;
    }
  diagnostic->kind = kind;
}

/* Return a malloc'd string describing a location.  The caller is
   responsible for freeing the memory.  */
char *
diagnostic_build_prefix (diagnostic)
     diagnostic_info *diagnostic;
{
  static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T) (T),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
    "must-not-happen"
  };
   if (diagnostic->kind >= DK_LAST_DIAGNOSTIC_KIND)
     abort();

  return diagnostic->location.file
    ? build_message_string ("%s:%d: %s",
                            diagnostic->location.file,
                            diagnostic->location.line,
                            _(diagnostic_kind_text[diagnostic->kind]))
    : build_message_string ("%s: %s", progname,
                            _(diagnostic_kind_text[diagnostic->kind]));
}

/* Report a diagnostic MESSAGE at the declaration DECL.
   MSG is a format string which uses %s to substitute the declaration
   name; subsequent substitutions are a la output_format.  */
static void
diagnostic_for_decl (diagnostic, decl)
     diagnostic_info *diagnostic;
     tree decl;
{
  if (global_dc->lock++)
    error_recursion (global_dc);

  if (diagnostic_count_diagnostic (global_dc, diagnostic->kind))
    {
      diagnostic_report_current_function (global_dc);
      output_set_prefix
	(&global_dc->buffer, diagnostic_build_prefix (diagnostic));
      format_with_decl (&global_dc->buffer, &diagnostic->message, decl);
      output_flush (&global_dc->buffer);
      output_destroy_prefix (&global_dc->buffer);
    }
  global_dc->lock--;
}

void
diagnostic_flush_buffer (context)
     diagnostic_context *context;
{
  output_buffer_to_stream (&context->buffer);
  fflush (output_buffer_attached_stream (&context->buffer));
}

/* Count a diagnostic.  Return true if the message should be printed.  */
bool
diagnostic_count_diagnostic (context, kind)
    diagnostic_context *context;
    diagnostic_t kind;
{
  switch (kind)
    {
    default:
      abort();
      break;

    case DK_FATAL: case DK_ICE: case DK_SORRY:
    case DK_ANACHRONISM: case DK_NOTE:
      ++diagnostic_kind_count (context, kind);
      break;

    case DK_WARNING:
      if (!diagnostic_report_warnings_p ())
        return false;
      else if (!warnings_are_errors)
        {
          ++diagnostic_kind_count (context, DK_WARNING);
          break;
        }
      /* else fall through.  */

    case DK_ERROR:
      if (kind == DK_WARNING && context->warnings_are_errors_message)
        {
	  output_verbatim (&context->buffer,
                           "%s: warnings being treated as errors\n", progname);
          context->warnings_are_errors_message = false;
        }
      ++diagnostic_kind_count (context, DK_ERROR);
      break;
    }

  return true;
}

/* Print a diagnostic MSGID on FILE.  This is just fprintf, except it
   runs its second argument through gettext.  */
void
fnotice VPARAMS ((FILE *file, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, FILE *, file);
  VA_FIXEDARG (ap, const char *, msgid);

  vfprintf (file, _(msgid), ap);
  VA_CLOSE (ap);
}


/* Print a fatal I/O error message.  Argument are like printf.
   Also include a system error message based on `errno'.  */
void
fatal_io_error VPARAMS ((const char *msgid, ...))
{
  text_info text;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  text.format_spec = _(msgid);
  text.args_ptr = &ap;
  output_printf (&global_dc->buffer, "%s: %s: ", progname, xstrerror (errno));
  output_format (&global_dc->buffer, &text);
  output_flush (&global_dc->buffer);
  VA_CLOSE (ap);
  exit (FATAL_EXIT_CODE);
}

/* Issue a pedantic warning MSGID.  */
void
pedwarn VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, _(msgid), &ap, input_filename, lineno,
                       pedantic_error_kind ());
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}

/* Issue a pedantic warning about DECL.  */
void
pedwarn_with_decl VPARAMS ((tree decl, const char *msgid, ...))
{
  diagnostic_info diagnostic;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, tree, decl);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, _(msgid), &ap,
                       DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl),
                       pedantic_error_kind ());

  /* We don't want -pedantic-errors to cause the compilation to fail from
     "errors" in system header files.  Sometimes fixincludes can't fix what's
     broken (eg: unsigned char bitfields - fixing it may change the alignment
     which will cause programs to mysteriously fail because the C library
     or kernel uses the original layout).  There's no point in issuing a
     warning either, it's just unnecessary noise.  */
  if (!DECL_IN_SYSTEM_HEADER (decl))
    diagnostic_for_decl (&diagnostic, decl);
  VA_CLOSE (ap);
}

/* Same as above but within the context FILE and LINE.  */
void
pedwarn_with_file_and_line VPARAMS ((const char *file, int line,
				     const char *msgid, ...))
{
  diagnostic_info diagnostic;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, file);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, _(msgid), &ap, file, line,
                       pedantic_error_kind ());
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}

/* Just apologize with MSGID.  */
void
sorry VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  ++sorrycount;
  diagnostic_set_info (&diagnostic, _(msgid), &ap,
                       input_filename, lineno, DK_SORRY);

  output_set_prefix
    (&global_dc->buffer, diagnostic_build_prefix (&diagnostic));
  output_format (&global_dc->buffer, &diagnostic.message);
  output_flush (&global_dc->buffer);
  VA_CLOSE (ap);
}

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */
void
announce_function (decl)
     tree decl;
{
  if (!quiet_flag)
    {
      if (rtl_dump_and_exit)
	verbatim ("%s ", IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	verbatim (" %s", (*lang_hooks.decl_printable_name) (decl, 2));
      fflush (stderr);
      output_needs_newline (&global_dc->buffer) = true;
      diagnostic_set_last_function (global_dc);
    }
}

/* The default function to print out name of current function that caused
   an error.  */
void
lhd_print_error_function (context, file)
     diagnostic_context *context;
     const char *file;
{
  if (diagnostic_last_function_changed (context))
    {
      const char *old_prefix = output_prefix (&context->buffer);
      char *new_prefix = file ? build_message_string ("%s: ", file) : NULL;

      output_set_prefix (&context->buffer, new_prefix);

      if (current_function_decl == NULL)
	output_add_string (&context->buffer, _("At top level:"));
      else
	{
	  if (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
	    output_printf
	      (&context->buffer, "In member function `%s':",
	       (*lang_hooks.decl_printable_name) (current_function_decl, 2));
	  else
	    output_printf
	      (&context->buffer, "In function `%s':",
	       (*lang_hooks.decl_printable_name) (current_function_decl, 2));
	}
      output_add_newline (&context->buffer);

      diagnostic_set_last_function (context);
      output_buffer_to_stream (&context->buffer);
      context->buffer.state.prefix = old_prefix;
      free ((char*) new_prefix);
    }
}

/* Prints out, if necessary, the name of the current function
  that caused an error.  Called from all error and warning functions.
  We ignore the FILE parameter, as it cannot be relied upon.  */

void
diagnostic_report_current_function (context)
     diagnostic_context *context;
{
  diagnostic_report_current_module (context);
  (*lang_hooks.print_error_function) (context, input_filename);
}

void
error_with_file_and_line VPARAMS ((const char *file, int line,
				   const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, file);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, file, line, DK_ERROR);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}

void
error_with_decl VPARAMS ((tree decl, const char *msgid, ...))
{
  diagnostic_info diagnostic;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, tree, decl);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap,
                       DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl),
                       DK_ERROR);
  diagnostic_for_decl (&diagnostic, decl);
  VA_CLOSE (ap);
}


/* Report an error message.  The arguments are like that of printf.  */

void
error VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       DK_ERROR);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}

/* Likewise, except that the compilation is terminated after printing the
   error message.  */

void
fatal_error VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       DK_FATAL);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);

  fnotice (stderr, "compilation terminated.\n");
  exit (FATAL_EXIT_CODE);
}

void
internal_error VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  if (global_dc->lock)
    error_recursion (global_dc);

#ifndef ENABLE_CHECKING
  if (errorcount > 0 || sorrycount > 0)
    {
      fnotice (stderr, "%s:%d: confused by earlier errors, bailing out\n",
	       input_filename, lineno);
      exit (FATAL_EXIT_CODE);
    }
#endif

  if (global_dc->internal_error != 0)
    (*global_dc->internal_error) (_(msgid), &ap);

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       DK_ICE);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);

  fnotice (stderr,
"Please submit a full bug report,\n\
with preprocessed source if appropriate.\n\
See %s for instructions.\n", bug_report_url);
  exit (FATAL_EXIT_CODE);
}

void
warning_with_file_and_line VPARAMS ((const char *file, int line,
				     const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, file);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, file, line, DK_WARNING);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}

void
warning_with_decl VPARAMS ((tree decl, const char *msgid, ...))
{
  diagnostic_info diagnostic;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, tree, decl);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap,
                       DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl),
                       DK_WARNING);
  diagnostic_for_decl (&diagnostic, decl);
  VA_CLOSE (ap);
}

void
warning VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       DK_WARNING);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}


/* Same as above but use diagnostic_buffer.  */

void
verbatim VPARAMS ((const char *msgid, ...))
{
  text_info text;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  text.format_spec = _(msgid);
  text.args_ptr = &ap;
  output_do_verbatim (&global_dc->buffer, &text);
  output_buffer_to_stream (&global_dc->buffer);
  VA_CLOSE (ap);
}

/* Report a diagnostic message (an error or a warning) as specified by
   DC.  This function is *the* subroutine in terms of which front-ends
   should implement their specific diagnostic handling modules.  The
   front-end independent format specifiers are exactly those described
   in the documentation of output_format.  */

void
diagnostic_report_diagnostic (context, diagnostic)
     diagnostic_context *context;
     diagnostic_info *diagnostic;
{
  if (context->lock++)
    error_recursion (context);

  if (diagnostic_count_diagnostic (context, diagnostic->kind))
    {
      (*diagnostic_starter (context)) (context, diagnostic);
      output_format (&context->buffer, &diagnostic->message);
      (*diagnostic_finalizer (context)) (context, diagnostic);
      output_flush (&context->buffer);
    }

  --context->lock;
}

/* Inform the user that an error occurred while trying to report some
   other error.  This indicates catastrophic internal inconsistencies,
   so give up now.  But do try to flush out the previous error.
   This mustn't use internal_error, that will cause infinite recursion.  */

static void
error_recursion (context)
     diagnostic_context *context;
{
  if (context->lock < 3)
    output_flush (&context->buffer);

  fnotice (stderr,
	   "Internal compiler error: Error reporting routines re-entered.\n");
  fnotice (stderr,
"Please submit a full bug report,\n\
with preprocessed source if appropriate.\n\
See %s for instructions.\n", bug_report_url);
  exit (FATAL_EXIT_CODE);
}

/* Given a partial pathname as input, return another pathname that
   shares no directory elements with the pathname of __FILE__.  This
   is used by fancy_abort() to print `Internal compiler error in expr.c'
   instead of `Internal compiler error in ../../GCC/gcc/expr.c'.  */

const char *
trim_filename (name)
     const char *name;
{
  static const char this_file[] = __FILE__;
  const char *p = name, *q = this_file;

  /* First skip any "../" in each filename.  This allows us to give a proper
     reference to a file in a subdirectory.  */
  while (p[0] == '.' && p[1] == '.'
	 && (p[2] == DIR_SEPARATOR
#ifdef DIR_SEPARATOR_2
	     || p[2] == DIR_SEPARATOR_2
#endif
	     ))
    p += 3;

  while (q[0] == '.' && q[1] == '.'
	 && (q[2] == DIR_SEPARATOR
#ifdef DIR_SEPARATOR_2
	     || p[2] == DIR_SEPARATOR_2
#endif
	     ))
    q += 3;

  /* Now skip any parts the two filenames have in common.  */
  while (*p == *q && *p != 0 && *q != 0)
    p++, q++;

  /* Now go backwards until the previous directory separator.  */
  while (p > name && p[-1] != DIR_SEPARATOR
#ifdef DIR_SEPARATOR_2
	 && p[-1] != DIR_SEPARATOR_2
#endif
	 )
    p--;

  return p;
}

/* Report an internal compiler error in a friendly manner and without
   dumping core.  */

void
fancy_abort (file, line, function)
     const char *file;
     int line;
     const char *function;
{
  internal_error ("in %s, at %s:%d", function, trim_filename (file), line);
}

void
diagnostic_report_current_module (context)
     diagnostic_context *context;
{
  struct file_stack *p;

  if (output_needs_newline (&context->buffer))
    {
      output_add_newline (&context->buffer);
      output_needs_newline (&context->buffer) = false;
    }

  if (input_file_stack && input_file_stack->next != 0
      && diagnostic_last_module_changed (context))
    {
      for (p = input_file_stack->next; p; p = p->next)
	if (p == input_file_stack->next)
	  output_verbatim (&context->buffer,
                           "In file included from %s:%d", p->name, p->line);
	else
	  output_verbatim (&context->buffer,
                           ",\n                 from %s:%d", p->name, p->line);
      output_verbatim (&context->buffer, ":\n");
      diagnostic_set_last_module (context);
    }
}

static void
default_diagnostic_starter (context, diagnostic)
     diagnostic_context *context;
     diagnostic_info *diagnostic;
{
  diagnostic_report_current_function (context);
  output_set_prefix (&context->buffer, diagnostic_build_prefix (diagnostic));
}

static void
default_diagnostic_finalizer (context, diagnostic)
     diagnostic_context *context;
     diagnostic_info *diagnostic __attribute__((unused));
{
  output_destroy_prefix (&context->buffer);
}

void
inform VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       DK_NOTE);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}

void
warn_deprecated_use (node)
     tree node;
{
  if (node == 0 || !warn_deprecated_decl)
    return;

  if (DECL_P (node))
    warning ("`%s' is deprecated (declared at %s:%d)",
	     IDENTIFIER_POINTER (DECL_NAME (node)),
	     DECL_SOURCE_FILE (node), DECL_SOURCE_LINE (node));
  else if (TYPE_P (node))
    {
      const char *what = NULL;
      tree decl = TYPE_STUB_DECL (node);

      if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	what = IDENTIFIER_POINTER (TYPE_NAME (node));
      else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
	       && DECL_NAME (TYPE_NAME (node)))
	what = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node)));

      if (what)
	{
	  if (decl)
	    warning ("`%s' is deprecated (declared at %s:%d)", what,
		     DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
	  else
	    warning ("`%s' is deprecated", what);
	}
      else if (decl)
	warning ("type is deprecated (declared at %s:%d)",
		 DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
      else
	warning ("type is deprecated");
    }
}
