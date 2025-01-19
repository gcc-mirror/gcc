/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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

#ifndef GCC_PRETTY_PRINT_H
#define GCC_PRETTY_PRINT_H

#include "obstack.h"
#include "rich-location.h"
#include "diagnostic-url.h"

/* Maximum number of format string arguments.  */
#define PP_NL_ARGMAX   30

/* The type of a text to be formatted according a format specification
   along with a list of things.  */
struct text_info
{
  text_info () = default;
  text_info (const char *format_spec,
	     va_list *args_ptr,
	     int err_no,
	     void **data = nullptr,
	     rich_location *rich_loc = nullptr)
  : m_format_spec (format_spec),
    m_args_ptr (args_ptr),
    m_err_no (err_no),
    m_data (data),
    m_richloc (rich_loc)
  {
  }

  void set_location (unsigned int idx, location_t loc,
		     enum range_display_kind range_display_kind);
  location_t get_location (unsigned int index_of_location) const;

  const char *m_format_spec;
  va_list *m_args_ptr;
  int m_err_no;  /* for %m */
  void **m_data;
  rich_location *m_richloc;
};

/* How often diagnostics are prefixed by their locations:
   o DIAGNOSTICS_SHOW_PREFIX_NEVER: never - not yet supported;
   o DIAGNOSTICS_SHOW_PREFIX_ONCE: emit only once;
   o DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE: emit each time a physical
   line is started.  */
enum diagnostic_prefixing_rule_t
{
  DIAGNOSTICS_SHOW_PREFIX_ONCE       = 0x0,
  DIAGNOSTICS_SHOW_PREFIX_NEVER      = 0x1,
  DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE = 0x2
};

class pp_formatted_chunks;
class output_buffer;
class pp_token_list;
class urlifier;

namespace pp_markup {
  class context;
} // namespace pp_markup

/* The output buffer datatype.  This is best seen as an abstract datatype
   whose fields should not be accessed directly by clients.  */
class output_buffer
{
public:
  output_buffer ();
  output_buffer (const output_buffer &) = delete;
  output_buffer (output_buffer &&) = delete;
  ~output_buffer ();
  output_buffer & operator= (const output_buffer &) = delete;
  output_buffer & operator= (output_buffer &&) = delete;

  pp_formatted_chunks *push_formatted_chunks ();
  void pop_formatted_chunks ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  /* Obstack where the text is built up.  */
  struct obstack m_formatted_obstack;

  /* Obstack containing a chunked representation of the format
     specification plus arguments.  */
  struct obstack m_chunk_obstack;

  /* Currently active obstack: one of the above two.  This is used so
     that the text formatters don't need to know which phase we're in.  */
  struct obstack *m_obstack;

  /* Topmost element in a stack of arrays of formatted chunks.
     These come from the chunk_obstack.  */
  pp_formatted_chunks *m_cur_formatted_chunks;

  /* Where to output formatted text.  */
  FILE *m_stream;

  /* The amount of characters output so far.  */
  int m_line_length;

  /* This must be large enough to hold any printed integer or
     floating-point value.  */
  char m_digit_buffer[128];

  /* Nonzero means that text should be flushed when
     appropriate. Otherwise, text is buffered until either
     pp_really_flush or pp_clear_output_area are called.  */
  bool m_flush_p;
};

/* Finishes constructing a NULL-terminated character string representing
   the buffered text.  */
inline const char *
output_buffer_formatted_text (output_buffer *buff)
{
  obstack_1grow (buff->m_obstack, '\0');
  return (const char *) obstack_base (buff->m_obstack);
}

/* Append to the output buffer a string specified by its
   STARTing character and LENGTH.  */
inline void
output_buffer_append_r (output_buffer *buff, const char *start, int length)
{
  gcc_checking_assert (start);
  obstack_grow (buff->m_obstack, start, length);
  for (int i = 0; i < length; i++)
    if (start[i] == '\n')
      buff->m_line_length = 0;
    else
      buff->m_line_length++;
}

/*  Return a pointer to the last character emitted in the
    output_buffer.  A NULL pointer means no character available.  */
inline const char *
output_buffer_last_position_in_text (const output_buffer *buff)
{
  const char *p = NULL;
  struct obstack *text = buff->m_obstack;

  if (obstack_base (text) != obstack_next_free (text))
    p = ((const char *) obstack_next_free (text)) - 1;
  return p;
}


/* The type of pretty-printer flags passed to clients.  */
typedef unsigned int pp_flags;

enum pp_padding
{
  pp_none, pp_before, pp_after
};

/* Structure for switching in and out of verbatim mode in a convenient
   manner.  */
struct pp_wrapping_mode_t
{
  /* Current prefixing rule.  */
  diagnostic_prefixing_rule_t rule;

  /* The ideal upper bound of number of characters per line, as suggested
     by front-end.  */
  int line_cutoff;
};

/* The type of a hook that formats client-specific data onto a pretty_printer.
   A client-supplied formatter returns true if everything goes well,
   otherwise it returns false.  */
typedef bool (*printer_fn) (pretty_printer *, text_info *, const char *,
			    int, bool, bool, bool, bool *, pp_token_list &);

/* Base class for an optional client-supplied object for doing additional
   processing between stages 2 and 3 of formatted printing.  */
class format_postprocessor
{
 public:
  virtual ~format_postprocessor () {}
  virtual format_postprocessor *clone() const = 0;
  virtual void handle (pretty_printer *) = 0;
};

/* Abstract base class for writing formatted tokens to the pretty_printer's
   text buffer, allowing for output formats and dumpfiles to override
   how different kinds of tokens are handled.  */

class token_printer
{
public:
  virtual ~token_printer () {}
  virtual void print_tokens (pretty_printer *pp,
			     const pp_token_list &tokens) = 0;
};

inline bool & pp_needs_newline (pretty_printer *pp);

/* True if PRETTY-PRINTER is in line-wrapping mode.  */
#define pp_is_wrapping_line(PP) (pp_line_cutoff (PP) > 0)

inline output_buffer *&pp_buffer (pretty_printer *pp);
inline output_buffer *pp_buffer (const pretty_printer *pp);
inline const char *pp_get_prefix (const pretty_printer *pp);
extern char *pp_take_prefix (pretty_printer *);
extern void pp_destroy_prefix (pretty_printer *);
inline int &pp_line_cutoff (pretty_printer *pp);
inline diagnostic_prefixing_rule_t &pp_prefixing_rule (pretty_printer *pp);
inline pp_wrapping_mode_t &pp_wrapping_mode (pretty_printer *pp);
inline int & pp_indentation (pretty_printer *pp);
inline bool & pp_translate_identifiers (pretty_printer *pp);
inline bool & pp_show_color (pretty_printer *pp);
inline printer_fn &pp_format_decoder (pretty_printer *pp);
inline format_postprocessor *& pp_format_postprocessor (pretty_printer *pp);
inline bool & pp_show_highlight_colors (pretty_printer *pp);

class urlifier;

/* The data structure that contains the bare minimum required to do
   proper pretty-printing.  Clients may derive from this structure
   and add additional fields they need.  */
class pretty_printer
{
public:
  friend inline output_buffer *&pp_buffer (pretty_printer *pp);
  friend inline output_buffer *pp_buffer (const pretty_printer *pp);
  friend inline const char *pp_get_prefix (const pretty_printer *pp);
  friend char *pp_take_prefix (pretty_printer *);
  friend void pp_destroy_prefix (pretty_printer *);
  friend inline int &pp_line_cutoff (pretty_printer *pp);
  friend inline diagnostic_prefixing_rule_t &
  pp_prefixing_rule (pretty_printer *pp);
  friend inline const diagnostic_prefixing_rule_t &
  pp_prefixing_rule (const pretty_printer *pp);
  friend inline pp_wrapping_mode_t &pp_wrapping_mode (pretty_printer *pp);
  friend bool & pp_needs_newline (pretty_printer *pp);
  friend int & pp_indentation (pretty_printer *pp);
  friend bool & pp_translate_identifiers (pretty_printer *pp);
  friend bool & pp_show_color (pretty_printer *pp);
  friend printer_fn &pp_format_decoder (pretty_printer *pp);
  friend format_postprocessor *& pp_format_postprocessor (pretty_printer *pp);
  friend bool & pp_show_highlight_colors (pretty_printer *pp);

  friend void pp_output_formatted_text (pretty_printer *,
					const urlifier *);

  /* Default construct a pretty printer with specified
     maximum line length cut off limit.  */
  explicit pretty_printer (int = 0);
  explicit pretty_printer (const pretty_printer &other);

  virtual ~pretty_printer ();

  virtual std::unique_ptr<pretty_printer> clone () const;

  void set_output_stream (FILE *outfile)
  {
    m_buffer->m_stream = outfile;
  }

  void set_token_printer (token_printer* tp)
  {
    m_token_printer = tp; // borrowed
  }

  void set_prefix (char *prefix);

  void emit_prefix ();

  void format (text_info &text);

  void maybe_space ();

  bool supports_urls_p () const { return m_url_format != URL_FORMAT_NONE; }
  diagnostic_url_format get_url_format () const { return m_url_format; }
  void set_url_format (diagnostic_url_format url_format)
  {
    m_url_format = url_format;
  }

  void begin_url (const char *url);
  void end_url ();

  /* Switch into verbatim mode and return the old mode.  */
  pp_wrapping_mode_t
  set_verbatim_wrapping ()
  {
    const pp_wrapping_mode_t oldmode = pp_wrapping_mode (this);
    pp_line_cutoff (this) = 0;
    pp_prefixing_rule (this) = DIAGNOSTICS_SHOW_PREFIX_NEVER;
    return oldmode;
  }

  void set_padding (pp_padding padding) { m_padding = padding; }
  pp_padding get_padding () const { return m_padding; }

  void clear_state ();
  void set_real_maximum_length ();
  int remaining_character_count_for_line ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

private:
  /* Where we print external representation of ENTITY.  */
  output_buffer *m_buffer;

  /* The prefix for each new line.  If non-NULL, this is "owned" by the
     pretty_printer, and will eventually be free-ed.  */
  char *m_prefix;

  /* Where to put whitespace around the entity being formatted.  */
  pp_padding m_padding;

  /* The real upper bound of number of characters per line, taking into
     account the case of a very very looong prefix.  */
  int m_maximum_length;

  /* Indentation count.  */
  int m_indent_skip;

  /* Current wrapping mode.  */
  pp_wrapping_mode_t m_wrapping;

  /* If non-NULL, this function formats a TEXT into the BUFFER.  When called,
     TEXT->format_spec points to a format code.  FORMAT_DECODER should call
     pp_string (and related functions) to add data to the BUFFER.
     FORMAT_DECODER can read arguments from *TEXT->args_pts using VA_ARG.
     If the BUFFER needs additional characters from the format string, it
     should advance the TEXT->format_spec as it goes.  When FORMAT_DECODER
     returns, TEXT->format_spec should point to the last character processed.
     The QUOTE and FORMATTED_TOKEN_LIST are passed in, to allow for
     deferring-handling of format codes (e.g. %H and %I in
     the C++ frontend).  */
  printer_fn m_format_decoder;

  /* If non-NULL, this is called by pp_format once after all format codes
     have been processed, to allow for client-specific postprocessing.
     This is used by the C++ frontend for handling the %H and %I
     format codes (which interract with each other).  */
  format_postprocessor *m_format_postprocessor;

  /* This is used by pp_output_formatted_text after it has converted all
     formatted chunks into a single list of tokens.
     Can be nullptr.
     Borrowed from the output format or from dump_pretty_printer.  */
  token_printer *m_token_printer;

  /* Nonzero if current PREFIX was emitted at least once.  */
  bool m_emitted_prefix;

  /* Nonzero means one should emit a newline before outputting anything.  */
  bool m_need_newline;

  /* Nonzero means identifiers are translated to the locale character
     set on output.  */
  bool m_translate_identifiers;

  /* Nonzero means that text should be colorized.  */
  bool m_show_color;

  /* True means that pertinent sections within the text should be
     highlighted with color.  */
  bool m_show_highlight_colors;

  /* Whether URLs should be emitted, and which terminator to use.  */
  diagnostic_url_format m_url_format;

  /* If true, then we've had a begin_url (nullptr), and so the
     next end_url should be a no-op.  */
  bool m_skipping_null_url;
};

inline output_buffer *&
pp_buffer (pretty_printer *pp)
{
  return pp->m_buffer;
}

inline output_buffer *
pp_buffer (const pretty_printer *pp)
{
  return pp->m_buffer;
}

inline const char *
pp_get_prefix (const pretty_printer *pp)
{
  return pp->m_prefix;
}

/* TRUE if a newline character needs to be added before further
   formatting.  */
inline bool &
pp_needs_newline (pretty_printer *pp)
{
  return pp->m_need_newline;
}

/* The amount of whitespace to be emitted when starting a new line.  */
inline int &
pp_indentation (pretty_printer *pp)
{
  return pp->m_indent_skip;
}

/* True if identifiers are translated to the locale character set on
   output.  */
inline bool &
pp_translate_identifiers (pretty_printer *pp)
{
  return pp->m_translate_identifiers;
}

/* True if colors should be shown.  */
inline bool &
pp_show_color (pretty_printer *pp)
{
  return pp->m_show_color;
}

inline printer_fn &
pp_format_decoder (pretty_printer *pp)
{
  return pp->m_format_decoder;
}

inline format_postprocessor *&
pp_format_postprocessor (pretty_printer *pp)
{
  return pp->m_format_postprocessor;
}

inline bool &
pp_show_highlight_colors (pretty_printer *pp)
{
  return pp->m_show_highlight_colors;
}

/* Maximum characters per line in automatic line wrapping mode.
   Zero means don't wrap lines.  */
inline int &
pp_line_cutoff (pretty_printer *pp)
{
  return pp->m_wrapping.line_cutoff;
}

/* Prefixing rule used in formatting a diagnostic message.  */
inline diagnostic_prefixing_rule_t &
pp_prefixing_rule (pretty_printer *pp)
{
  return pp->m_wrapping.rule;
}
inline const diagnostic_prefixing_rule_t &
pp_prefixing_rule (const pretty_printer *pp)
{
  return pp->m_wrapping.rule;
}

/* Get or set the wrapping mode as a single entity.  */
inline pp_wrapping_mode_t &
pp_wrapping_mode (pretty_printer *pp)
{
  return pp->m_wrapping;
}

#define pp_space(PP)            pp_character (PP, ' ')
#define pp_left_paren(PP)       pp_character (PP, '(')
#define pp_right_paren(PP)      pp_character (PP, ')')
#define pp_left_bracket(PP)     pp_character (PP, '[')
#define pp_right_bracket(PP)    pp_character (PP, ']')
#define pp_left_brace(PP)       pp_character (PP, '{')
#define pp_right_brace(PP)      pp_character (PP, '}')
#define pp_semicolon(PP)        pp_character (PP, ';')
#define pp_comma(PP)            pp_character (PP, ',')
#define pp_dot(PP)              pp_character (PP, '.')
#define pp_colon(PP)            pp_character (PP, ':')
#define pp_colon_colon(PP)      pp_string (PP, "::")
#define pp_arrow(PP)            pp_string (PP, "->")
#define pp_equal(PP)            pp_character (PP, '=')
#define pp_question(PP)         pp_character (PP, '?')
#define pp_bar(PP)              pp_character (PP, '|')
#define pp_bar_bar(PP)          pp_string (PP, "||")
#define pp_carret(PP)           pp_character (PP, '^')
#define pp_ampersand(PP)        pp_character (PP, '&')
#define pp_ampersand_ampersand(PP) pp_string (PP, "&&")
#define pp_less(PP)             pp_character (PP, '<')
#define pp_less_equal(PP)       pp_string (PP, "<=")
#define pp_greater(PP)          pp_character (PP, '>')
#define pp_greater_equal(PP)    pp_string (PP, ">=")
#define pp_plus(PP)             pp_character (PP, '+')
#define pp_minus(PP)            pp_character (PP, '-')
#define pp_star(PP)             pp_character (PP, '*')
#define pp_slash(PP)            pp_character (PP, '/')
#define pp_modulo(PP)           pp_character (PP, '%')
#define pp_exclamation(PP)      pp_character (PP, '!')
#define pp_complement(PP)       pp_character (PP, '~')
#define pp_quote(PP)            pp_character (PP, '\'')
#define pp_backquote(PP)        pp_character (PP, '`')
#define pp_doublequote(PP)      pp_character (PP, '"')
#define pp_underscore(PP)       pp_character (PP, '_')
#define pp_maybe_newline_and_indent(PP, N) \
  if (pp_needs_newline (PP)) pp_newline_and_indent (PP, N)
#define pp_scalar(PP, FORMAT, SCALAR)	                      \
  do					        	      \
    {			         			      \
      sprintf (pp_buffer (PP)->m_digit_buffer, FORMAT, SCALAR); \
      pp_string (PP, pp_buffer (PP)->m_digit_buffer);           \
    }						              \
  while (0)
#define pp_decimal_int(PP, I)  pp_scalar (PP, "%d", I)
#define pp_unsigned_wide_integer(PP, I) \
   pp_scalar (PP, HOST_WIDE_INT_PRINT_UNSIGNED, (unsigned HOST_WIDE_INT) I)
#define pp_vrange(PP, R)					\
  do								\
    {								\
      vrange_printer vrange_pp (PP);				\
      (R)->accept (vrange_pp);					\
    }								\
  while (0)
#define pp_double(PP, F)       pp_scalar (PP, "%f", F)
#define pp_pointer(PP, P)      pp_scalar (PP, "%p", P)

#define pp_identifier(PP, ID)  pp_string (PP, (pp_translate_identifiers (PP) \
					  ? identifier_to_locale (ID)	\
					  : (ID)))


extern void pp_set_line_maximum_length (pretty_printer *, int);
inline void pp_set_prefix (pretty_printer *pp, char *prefix)
{
  pp->set_prefix (prefix);
}
extern void pp_clear_output_area (pretty_printer *);
extern const char *pp_formatted_text (pretty_printer *);
extern const char *pp_last_position_in_text (const pretty_printer *);
inline void pp_emit_prefix (pretty_printer *pp)
{
  pp->emit_prefix ();
}
extern void pp_append_text (pretty_printer *, const char *, const char *);
extern void pp_newline_and_flush (pretty_printer *);
extern void pp_newline_and_indent (pretty_printer *, int);
extern void pp_separate_with (pretty_printer *, char);

/* If we haven't already defined a front-end-specific diagnostics
   style, use the generic one.  */
#ifdef GCC_DIAG_STYLE
#define GCC_PPDIAG_STYLE GCC_DIAG_STYLE
#else
#define GCC_PPDIAG_STYLE __gcc_diag__
#endif

/* This header may be included before diagnostics-core.h, hence the duplicate
   definitions to allow for GCC-specific formats.  */
#if GCC_VERSION >= 3005
#define ATTRIBUTE_GCC_PPDIAG(m, n) __attribute__ ((__format__ (GCC_PPDIAG_STYLE, m ,n))) ATTRIBUTE_NONNULL(m)
#else
#define ATTRIBUTE_GCC_PPDIAG(m, n) ATTRIBUTE_NONNULL(m)
#endif
extern void pp_printf (pretty_printer *, const char *, ...)
     ATTRIBUTE_GCC_PPDIAG(2,3);

extern void pp_printf_n (pretty_printer *, unsigned HOST_WIDE_INT n,
			 const char *, const char *, ...)
     ATTRIBUTE_GCC_PPDIAG(3,5)
     ATTRIBUTE_GCC_PPDIAG(4,5);

extern void pp_verbatim (pretty_printer *, const char *, ...)
     ATTRIBUTE_GCC_PPDIAG(2,3);
extern void pp_flush (pretty_printer *);
extern void pp_really_flush (pretty_printer *);
inline void pp_format (pretty_printer *pp, text_info *text)
{
  gcc_assert (text);
  pp->format (*text);
}
extern void pp_output_formatted_text (pretty_printer *,
				      const urlifier * = nullptr);
extern void pp_format_verbatim (pretty_printer *, text_info *);

extern void pp_indent (pretty_printer *);
extern void pp_newline (pretty_printer *);
extern void pp_character (pretty_printer *, int);
extern void pp_string (pretty_printer *, const char *);
extern void pp_string_n (pretty_printer *, const char *, size_t);
extern void pp_unicode_character (pretty_printer *, unsigned);

extern void pp_write_text_to_stream (pretty_printer *);
extern void pp_write_text_as_dot_label_to_stream (pretty_printer *, bool);
extern void pp_write_text_as_html_like_dot_to_stream (pretty_printer *pp);

inline void pp_maybe_space (pretty_printer *pp)
{
  pp->maybe_space ();
}

extern void pp_begin_quote (pretty_printer *, bool);
extern void pp_end_quote (pretty_printer *, bool);

inline void
pp_begin_url (pretty_printer *pp, const char *url)
{
  pp->begin_url (url);
}

inline void
pp_end_url (pretty_printer *pp)
{
  pp->end_url ();
}

/* Switch into verbatim mode and return the old mode.  */
inline pp_wrapping_mode_t
pp_set_verbatim_wrapping (pretty_printer *pp)
{
  return pp->set_verbatim_wrapping ();
}

extern const char *identifier_to_locale (const char *);
extern void *(*identifier_to_locale_alloc) (size_t);
extern void (*identifier_to_locale_free) (void *);

/* Print I to PP in decimal.  */

inline void
pp_wide_integer (pretty_printer *pp, HOST_WIDE_INT i)
{
  pp_scalar (pp, HOST_WIDE_INT_PRINT_DEC, i);
}

inline void
pp_wide_int (pretty_printer *pp, const wide_int_ref &w, signop sgn)
{
  unsigned int len;
  print_dec_buf_size (w, sgn, &len);
  if (UNLIKELY (len > sizeof (pp_buffer (pp)->m_digit_buffer)))
    pp_wide_int_large (pp, w, sgn);
  else
    {
      print_dec (w, pp_buffer (pp)->m_digit_buffer, sgn);
      pp_string (pp, pp_buffer (pp)->m_digit_buffer);
    }
}

template<unsigned int N, typename T>
void pp_wide_integer (pretty_printer *pp, const poly_int<N, T> &);

#endif /* GCC_PRETTY_PRINT_H */
