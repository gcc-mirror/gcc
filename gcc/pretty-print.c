/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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

#include "config.h"
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free

/* A pointer to the formatted diagnostic message.  */
#define pp_formatted_text_data(PP) \
   ((const char *) obstack_base (&pp_base (PP)->buffer->obstack))

/* Format an integer given by va_arg (ARG, type-specifier T) where
   type-specifier is a precision modifier as indicated by PREC.  F is
   a string used to construct the appropriate format-specifier.  */
#define pp_integer_with_precision(PP, ARG, PREC, T, F)       \
  do                                                         \
    switch (PREC)                                            \
      {                                                      \
      case 0:                                                \
        pp_scalar (PP, "%" F, va_arg (ARG, T));              \
        break;                                               \
                                                             \
      case 1:                                                \
        pp_scalar (PP, "%l" F, va_arg (ARG, long T));        \
        break;                                               \
                                                             \
      case 2:                                                \
        pp_scalar (PP, "%ll" F, va_arg (ARG, long long T));  \
        break;                                               \
                                                             \
      default:                                               \
        break;                                               \
      }                                                      \
  while (0)


/* Subroutine of pp_set_maximum_length.  Set up PRETTY-PRINTER's
   internal maximum characters per line.  */
static void
pp_set_real_maximum_length (pretty_printer *pp)
{
  /* If we're told not to wrap lines then do the obvious thing.  In case
     we'll emit prefix only once per message, it is appropriate
     not to increase unnecessarily the line-length cut-off.  */
  if (!pp_is_wrapping_line (pp)
      || pp_prefixing_rule (pp) == DIAGNOSTICS_SHOW_PREFIX_ONCE
      || pp_prefixing_rule (pp) == DIAGNOSTICS_SHOW_PREFIX_NEVER)
    pp->maximum_length = pp_line_cutoff (pp);
  else
    {
      int prefix_length = pp->prefix ? strlen (pp->prefix) : 0;
      /* If the prefix is ridiculously too long, output at least
         32 characters.  */
      if (pp_line_cutoff (pp) - prefix_length < 32)
	pp->maximum_length = pp_line_cutoff (pp) + 32;
      else
	pp->maximum_length = pp_line_cutoff (pp);
    }
}

/* Clear PRETTY-PRINTER's output state.  */
static inline void
pp_clear_state (pretty_printer *pp)
{
  pp->emitted_prefix = false;
  pp_indentation (pp) = 0;
}

/* Flush the formatted text of PRETTY-PRINTER onto the attached stream.  */
static inline void
pp_write_text_to_stream (pretty_printer *pp)
{
  const char *text = pp_formatted_text (pp);
  fputs (text, pp->buffer->stream);
  pp_clear_output_area (pp);
}

/* Wrap a text delimited by START and END into PRETTY-PRINTER.  */
static void
pp_wrap_text (pretty_printer *pp, const char *start, const char *end)
{
  bool wrapping_line = pp_is_wrapping_line (pp);

  while (start != end)
    {
      /* Dump anything bordered by whitespaces.  */
      {
	const char *p = start;
	while (p != end && !ISBLANK (*p) && *p != '\n')
	  ++p;
	if (wrapping_line
            && p - start >= pp_remaining_character_count_for_line (pp))
	  pp_newline (pp);
	pp_append_text (pp, start, p);
	start = p;
      }

      if (start != end && ISBLANK (*start))
	{
	  pp_space (pp);
	  ++start;
	}
      if (start != end && *start == '\n')
	{
	  pp_newline (pp);
	  ++start;
	}
    }
}

/* Same as pp_wrap_text but wrap text only when in line-wrapping mode.  */
static inline void
pp_maybe_wrap_text (pretty_printer *pp, const char *start, const char *end)
{
  if (pp_is_wrapping_line (pp))
    pp_wrap_text (pp, start, end);
  else
    pp_append_text (pp, start, end);
}

/* Append to the output area of PRETTY-PRINTER a string specified by its
   STARTing character and LENGTH.  */
static inline void
pp_append_r (pretty_printer *pp, const char *start, int length)
{
  obstack_grow (&pp->buffer->obstack, start, length);
  pp->buffer->line_length += length;
}

/* Insert enough spaces into the output area of PRETTY-PRINTER to bring
   the column position to the current indentation level, assuming that a
   newline has just been written to the buffer.  */
void
pp_base_indent (pretty_printer *pp)
{
  int n = pp_indentation (pp);
  int i;

  for (i = 0; i < n; ++i)
    pp_space (pp);
}

/* Format a message pointed to by TEXT.  The following format specifiers are
   recognized as being client independent:
   %d, %i: (signed) integer in base ten.
   %u: unsigned integer in base ten.
   %o: unsigned integer in base eight.
   %x: unsigned integer in base sixteen.
   %ld, %li, %lo, %lu, %lx: long versions of the above.
   %lld, %lli, %llo, %llu, %llx: long long versions.
   %wd, %wi, %wo, %wu, %wx: HOST_WIDE_INT versions.
   %c: character.
   %s: string.
   %p: pointer.
   %m: strerror(text->err_no) - does not consume a value from args_ptr.
   %%: `%'.
   %*.s: a substring the length of which is specified by an integer.
   %H: location_t.  */
void
pp_base_format_text (pretty_printer *pp, text_info *text)
{
  for (; *text->format_spec; ++text->format_spec)
    {
      int precision = 0;
      bool wide = false;

      /* Ignore text.  */
      {
	const char *p = text->format_spec;
	while (*p && *p != '%')
	  ++p;
	pp_wrap_text (pp, text->format_spec, p);
        text->format_spec = p;
      }

      if (*text->format_spec == '\0')
	break;

      /* We got a '%'.  Parse precision modifiers, if any.  */
      switch (*++text->format_spec)
        {
        case 'w':
          wide = true;
          ++text->format_spec;
          break;

        case 'l':
          do
            ++precision;
          while (*++text->format_spec == 'l');
          break;

        default:
          break;
        }
      /* We don't support precision beyond that of "long long".  */
      if (precision > 2)
        abort();

      switch (*text->format_spec)
	{
	case 'c':
	  pp_character (pp, va_arg (*text->args_ptr, int));
	  break;

	case 'd':
	case 'i':
          if (wide)
            pp_wide_integer (pp, va_arg (*text->args_ptr, HOST_WIDE_INT));
          else
            pp_integer_with_precision
              (pp, *text->args_ptr, precision, int, "d");
	  break;

	case 'o':
          if (wide)
            pp_scalar (pp, "%" HOST_WIDE_INT_PRINT "o",
                       va_arg (*text->args_ptr, unsigned HOST_WIDE_INT));
          else
            pp_integer_with_precision
              (pp, *text->args_ptr, precision, unsigned, "u");
	  break;

	case 's':
	  pp_string (pp, va_arg (*text->args_ptr, const char *));
	  break;

        case 'p':
          pp_pointer (pp, va_arg (*text->args_ptr, void *));
          break;

	case 'u':
          if (wide)
            pp_scalar (pp, HOST_WIDE_INT_PRINT_UNSIGNED,
                       va_arg (*text->args_ptr, unsigned HOST_WIDE_INT));
          else
            pp_integer_with_precision
              (pp, *text->args_ptr, precision, unsigned, "u");
	  break;

	case 'x':
          if (wide)
            pp_scalar (pp, HOST_WIDE_INT_PRINT_HEX,
                       va_arg (*text->args_ptr, unsigned HOST_WIDE_INT));
          else
            pp_integer_with_precision
              (pp, *text->args_ptr, precision, unsigned, "x");
	  break;

	case 'm':
	  pp_string (pp, xstrerror (text->err_no));
	  break;

	case '%':
	  pp_character (pp, '%');
	  break;

        case 'H':
          {
            const location_t *locus = va_arg (*text->args_ptr, location_t *);
            pp_string (pp, "file '");
            pp_string (pp, locus->file);
            pp_string (pp, "', line ");
            pp_decimal_int (pp, locus->line);
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
	    pp_append_text (pp, s, s + n);
	  }
	  break;

	default:
          if (!pp_format_decoder (pp) || !(*pp_format_decoder (pp)) (pp, text))
	    {
	      /* Hmmm.  The client failed to install a format translator
                 but called us with an unrecognized format.  Or, maybe, the
                 translated string just contains an invalid format, or
                 has formats in the wrong order.  Sorry.  */
	      abort ();
	    }
	}
    }
}

/* Helper subroutine of output_verbatim and verbatim. Do the appropriate
   settings needed by BUFFER for a verbatim formatting.  */
void
pp_base_format_verbatim (pretty_printer *pp, text_info *text)
{
  diagnostic_prefixing_rule_t rule = pp_prefixing_rule (pp);
  int line_cutoff = pp_line_cutoff (pp);

  /* Set verbatim mode.  */
  pp->prefixing_rule = DIAGNOSTICS_SHOW_PREFIX_NEVER;
  pp_line_cutoff (pp) = 0;
  /* Do the actual formatting.  */
  pp_format_text (pp, text);
  /* Restore previous settings.  */
  pp_prefixing_rule (pp) = rule;
  pp_line_cutoff (pp) = line_cutoff;
}

/* Flush the content of BUFFER onto the attached stream.  */
void
pp_base_flush (pretty_printer *pp)
{
  pp_write_text_to_stream (pp);
  pp_clear_state (pp);
  fputc ('\n', pp->buffer->stream);
  fflush (pp->buffer->stream);
  pp_needs_newline (pp) = false;
}

/* Sets the number of maximum characters per line PRETTY-PRINTER can
   output in line-wrapping mode.  A LENGTH value 0 suppresses
   line-wrapping.  */
void
pp_base_set_line_maximum_length (pretty_printer *pp, int length)
{
  pp_line_cutoff (pp) = length;
  pp_set_real_maximum_length (pp);
}

/* Clear PRETTY-PRINTER output area text info.  */
void
pp_base_clear_output_area (pretty_printer *pp)
{
  obstack_free (&pp->buffer->obstack, obstack_base (&pp->buffer->obstack));
  pp->buffer->line_length = 0;
}

/* Set PREFIX for PRETTY-PRINTER.  */
void
pp_base_set_prefix (pretty_printer *pp, const char *prefix)
{
  pp->prefix = prefix;
  pp_set_real_maximum_length (pp);
  pp->emitted_prefix = false;
  pp_indentation (pp) = 0;
}

/* Free PRETTY-PRINTER's prefix, a previously malloc()'d string.  */
void
pp_base_destroy_prefix (pretty_printer *pp)
{
  if (pp->prefix != NULL)
    {
      free ((char *) pp->prefix);
      pp->prefix = NULL;
    }
}

/* Write out PRETTY-PRINTER's prefix.  */
void
pp_base_emit_prefix (pretty_printer *pp)
{
  if (pp->prefix != NULL)
    {
      switch (pp_prefixing_rule (pp))
	{
	default:
	case DIAGNOSTICS_SHOW_PREFIX_NEVER:
	  break;

	case DIAGNOSTICS_SHOW_PREFIX_ONCE:
	  if (pp->emitted_prefix)
	    {
	      pp_base_indent (pp);
	      break;
	    }
	  pp_indentation (pp) += 3;
	  /* Fall through.  */

	case DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE:
	  {
	    int prefix_length = strlen (pp->prefix);
	    pp_append_r (pp, pp->prefix, prefix_length);
	    pp->emitted_prefix = true;
	  }
	  break;
	}
    }
}

/* Construct a PRETTY-PRINTER with PREFIX and of MAXIMUM_LENGTH
   characters per line.  */
void
pp_construct (pretty_printer *pp, const char *prefix, int maximum_length)
{
  memset (pp, 0, sizeof (pretty_printer));
  pp->buffer = xcalloc (1, sizeof (output_buffer));
  obstack_init (&pp->buffer->obstack);
  pp->buffer->stream = stderr;
  pp_line_cutoff (pp) = maximum_length;
  pp_prefixing_rule (pp) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
  pp_set_prefix (pp, prefix);
}

/* Append a string delimited by START and END to the output area of
   PRETTY-PRINTER.  No line wrapping is done.  However, if beginning a
   new line then emit PRETTY-PRINTER's prefix and skip any leading
   whitespace if appropriate.  The caller must ensure that it is
   safe to do so.  */
void
pp_base_append_text (pretty_printer *pp, const char *start, const char *end)
{
  /* Emit prefix and skip whitespace if we're starting a new line.  */
  if (pp->buffer->line_length == 0)
    {
      pp_emit_prefix (pp);
      if (pp_is_wrapping_line (pp))
	while (start != end && *start == ' ')
	  ++start;
    }
  pp_append_r (pp, start, end - start);
}

/* Finishes constructing a NULL-terminated character string representing
   the PRETTY-PRINTED text.  */
const char *
pp_base_formatted_text (pretty_printer *pp)
{
  obstack_1grow (&pp->buffer->obstack, '\0');
  return pp_formatted_text_data (pp);
}

/*  Return a pointer to the last character emitted in PRETTY-PRINTER's
    output area.  A NULL pointer means no character available.  */
const char *
pp_base_last_position_in_text (const pretty_printer *pp)
{
  const char *p = NULL;
  struct obstack *text = &pp->buffer->obstack;

  if (obstack_base (text) != obstack_next_free (text))
    p = ((const char *) obstack_next_free (text)) - 1;
  return p;
}

/* Return the amount of characters PRETTY-PRINTER can accept to
   make a full line.  Meaningful only in line-wrapping mode.  */
int
pp_base_remaining_character_count_for_line (pretty_printer *pp)
{
  return pp->maximum_length - pp->buffer->line_length;
}


/* Format a message into BUFFER a la printf.  */
void
pp_printf (pretty_printer *pp, const char *msg, ...)
{
  text_info text;
  va_list ap;

  va_start (ap, msg);
  text.err_no = errno;
  text.args_ptr = &ap;
  text.format_spec = msg;
  pp_format_text (pp, &text);
  va_end (ap);
}


/* Output MESSAGE verbatim into BUFFER.  */
void
pp_verbatim (pretty_printer *pp, const char *msg, ...)
{
  text_info text;
  va_list ap;

  va_start (ap, msg);
  text.err_no = errno;
  text.args_ptr = &ap;
  text.format_spec = msg;
  pp_format_verbatim (pp, &text);
  va_end (ap);
}



/* Have PRETTY-PRINTER start a new line.  */
void
pp_base_newline (pretty_printer *pp)
{
  obstack_1grow (&pp->buffer->obstack, '\n');
  pp->buffer->line_length = 0;
}

/* Have PRETTY-PRINTER add a CHARACTER.  */
void
pp_base_character (pretty_printer *pp, int c)
{
  if (pp_is_wrapping_line (pp)
      && pp_remaining_character_count_for_line (pp) <= 0)
    {
      pp_newline (pp);
      if (ISSPACE (c))
        return;
    }
  obstack_1grow (&pp->buffer->obstack, c);
  ++pp->buffer->line_length;
}

/* Append a STRING to the output area of PRETTY-PRINTER; the STRING may
   be line-wrapped if in appropriate mode.  */
void
pp_base_string (pretty_printer *pp, const char *str)
{
  pp_maybe_wrap_text (pp, str, str + (str ? strlen (str) : 0));
}


