/* Implementation detail of pp_format.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
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

#ifndef GCC_PRETTY_PRINT_FORMAT_IMPL_H
#define GCC_PRETTY_PRINT_FORMAT_IMPL_H

#include "pretty-print.h"
#include "diagnostic-event-id.h"

/* A struct representing a pending item to be printed within
   pp_format.

   These can represent:
   - a run of text within one of the output_buffers's obstacks
   - begin/end named color
   - open/close quote
   - begin/end URL
   - event IDs
   - custom data (for the formatter, for the pretty_printer,
     or the output format)

   These are built into pp_token_list instances.

   Doing so allows for interaction between:

   - pretty_printer formatting codes (such as C++'s %H and %I,
   which can't be printed until we've seen both)

   - output formats, such as text vs SARIF (so each can handle URLs
   and event IDs it its own way)

   - optimization records, where we want to stash data into the
   formatted messages

   - urlifiers: these can be run in phase 3 of formatting

   without needing lots of fragile logic on char pointers.

   To avoid needing lots of heap allocation/deallocation, pp_token
   instances are allocated in the pretty_printer's chunk_obstack:
   they must not outlive phase 3 of formatting of the given
   pp_formatted_chunks level.  */

struct pp_token
{
public:
  enum class kind
  {
    text,

    begin_color,
    end_color,

    begin_quote,
    end_quote,

    begin_url,
    end_url,

    event_id,

    custom_data,

    NUM_KINDS
  };

  pp_token (enum kind k);

  pp_token (const pp_token &) = delete;
  pp_token (pp_token &&) = delete;

  virtual ~pp_token () = default;

  pp_token &operator= (const pp_token &) = delete;
  pp_token &operator= (pp_token &&) = delete;

  void dump (FILE *out) const;
  void DEBUG_FUNCTION dump () const { dump (stderr); }

  static void *operator new (size_t sz, obstack &s);
  static void operator delete (void *);

  enum kind m_kind;

  // Intrusive doubly-linked list
  pp_token *m_prev;
  pp_token *m_next;
};

/* Subclasses of pp_token for the various kinds of token.  */

struct pp_token_text : public pp_token
{
  pp_token_text (label_text &&value)
  : pp_token (kind::text),
    m_value (std::move (value))
  {
    gcc_assert (m_value.get ());
  }

  label_text m_value;
};

template <>
template <>
inline bool
is_a_helper <pp_token_text *>::test (pp_token *tok)
{
  return tok->m_kind == pp_token::kind::text;
}

template <>
template <>
inline bool
is_a_helper <const pp_token_text *>::test (const pp_token *tok)
{
  return tok->m_kind == pp_token::kind::text;
}

struct pp_token_begin_color : public pp_token
{
  pp_token_begin_color (label_text &&value)
  : pp_token (kind::begin_color),
    m_value (std::move (value))
  {
    gcc_assert (m_value.get ());
  }

  label_text m_value;
};

template <>
template <>
inline bool
is_a_helper <pp_token_begin_color *>::test (pp_token *tok)
{
  return tok->m_kind == pp_token::kind::begin_color;
}

template <>
template <>
inline bool
is_a_helper <const pp_token_begin_color *>::test (const pp_token *tok)
{
  return tok->m_kind == pp_token::kind::begin_color;
}

struct pp_token_end_color : public pp_token
{
  pp_token_end_color ()
  : pp_token (kind::end_color)
  {
  }
};

struct pp_token_begin_quote : public pp_token
{
  pp_token_begin_quote ()
  : pp_token (kind::begin_quote)
  {
  }
};

struct pp_token_end_quote : public pp_token
{
  pp_token_end_quote ()
  : pp_token (kind::end_quote)
  {
  }
};

struct pp_token_begin_url : public pp_token
{
  pp_token_begin_url (label_text &&value)
  : pp_token (kind::begin_url),
    m_value (std::move (value))
  {
    gcc_assert (m_value.get ());
  }

  label_text m_value;
};

template <>
template <>
inline bool
is_a_helper <pp_token_begin_url*>::test (pp_token *tok)
{
  return tok->m_kind == pp_token::kind::begin_url;
}

template <>
template <>
inline bool
is_a_helper <const pp_token_begin_url*>::test (const pp_token *tok)
{
  return tok->m_kind == pp_token::kind::begin_url;
}

struct pp_token_end_url : public pp_token
{
  pp_token_end_url ()
    : pp_token (kind::end_url)
  {
  }
};

struct pp_token_event_id : public pp_token
{
  pp_token_event_id (diagnostic_event_id_t event_id)
  : pp_token (kind::event_id),
    m_event_id (event_id)
  {
    gcc_assert (event_id.known_p ());
  }

  diagnostic_event_id_t m_event_id;
};

template <>
template <>
inline bool
is_a_helper <pp_token_event_id *>::test (pp_token *tok)
{
  return tok->m_kind == pp_token::kind::event_id;
}

template <>
template <>
inline bool
is_a_helper <const pp_token_event_id *>::test (const pp_token *tok)
{
  return tok->m_kind == pp_token::kind::event_id;
}

struct pp_token_custom_data : public pp_token
{
  class value
  {
  public:
    virtual ~value () {}
    virtual void dump (FILE *out) const = 0;

    /* Hook for lowering a custom_data token to standard tokens.
       Return true and write to OUT if possible.
       Return false for custom_data that is to be handled by
       the token_printer.  */
    virtual bool as_standard_tokens (pp_token_list &out) = 0;
  };

  pp_token_custom_data (std::unique_ptr<value> val)
  : pp_token (kind::custom_data),
    m_value (std::move (val))
  {
    gcc_assert (m_value.get ());
  }

  std::unique_ptr<value> m_value;
};

template <>
template <>
inline bool
is_a_helper <pp_token_custom_data *>::test (pp_token *tok)
{
  return tok->m_kind == pp_token::kind::custom_data;
}

template <>
template <>
inline bool
is_a_helper <const pp_token_custom_data *>::test (const pp_token *tok)
{
  return tok->m_kind == pp_token::kind::custom_data;
}

/* A list of pp_token, with ownership of the tokens, using
   a particular obstack to allocate its tokens.  These are
   also allocated on the obstack during formatting (or, occasionally,
   the stack).  */

class pp_token_list
{
public:
  // Allocate a new pp_token_list within S.
  static pp_token_list *make (obstack &s)
  {
    return new (s) pp_token_list (s);
  }
  static void *operator new (size_t sz, obstack &s);
  static void operator delete (void *);

  pp_token_list (obstack &s);
  pp_token_list (const pp_token_list &) = delete;
  pp_token_list (pp_token_list &&);

  ~pp_token_list ();

  pp_token &operator= (const pp_token_list &) = delete;
  pp_token &operator= (pp_token_list &&) = delete;

/* Make a pp_token of the given subclass, using the relevant obstack to provide
   the memory.  The pp_token must therefore not outlive the current
   pp_formatted_chunks level during formatting.  */
  template<typename Subclass, typename... Args>
  std::unique_ptr<pp_token>
  make_token (Args&&... args)
  {
    return std::unique_ptr<pp_token>
      (new (m_obstack) Subclass (std::forward<Args> (args)...));
  }

  template<typename Subclass, typename... Args>
  void push_back (Args&&... args)
  {
    auto tok = make_token<Subclass> (std::forward<Args> (args)...);
    push_back (std::move (tok));
  }
  void push_back_text (label_text &&text);
  void push_back (std::unique_ptr<pp_token> tok);
  void push_back_list (pp_token_list &&list);

  std::unique_ptr<pp_token> pop_front ();

  std::unique_ptr<pp_token> remove_token (pp_token *tok);

  void insert_after (std::unique_ptr<pp_token> new_tok,
		     pp_token *relative_tok);

  void replace_custom_tokens ();
  void merge_consecutive_text_tokens ();
  void apply_urlifier (const urlifier &urlifier);

  void dump (FILE *out) const;
  void DEBUG_FUNCTION dump () const { dump (stderr); }

  obstack &m_obstack;

  pp_token *m_first;
  pp_token *m_end;
};

/* The pp_formatted_chunks data structure forms a stack of the results from the
   first phase of formatting (pp_format) which have not yet been
   output (pp_output_formatted_text).  A stack is necessary because
   the diagnostic starter may decide to generate its own output by way
   of the formatter.  */
class pp_formatted_chunks
{
  friend class pretty_printer;
  friend class pp_markup::context;
  friend class output_buffer;

public:
  pp_token_list * const * get_token_lists () const { return m_args; }

  void append_formatted_chunk (obstack &s, const char *content);

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  // For use in selftests
  pp_formatted_chunks *get_prev () const { return m_prev; }

private:
  /* Pointer to previous level on the stack.  */
  pp_formatted_chunks *m_prev;

  /* Array of chunks to output.  Each chunk is a doubly-linked list of
     pp_token.

     The chunks can be printed via pp_formatted_chunks::dump ().

     In the first phase of formatting, even-numbered chunks are
     to be output verbatim, odd-numbered chunks are format specifiers.
     For example, given:
       pp_format (pp,
		  "foo: %i, bar: %s, opt: %qs",
		  42, "baz", "-foption");

     after phase 1 we might have:
       (gdb) call buffer->cur_chunk_array->dump()
       0: [TEXT("foo: ")]
       1: [TEXT("i")]
       2: [TEXT(", bar: ")]
       3: [TEXT("s")]
       4: [TEXT(", opt: ")]
       5: [TEXT("qs")]

     The second phase replaces all odd-numbered chunks with formatted
     token lists.  In the above example, after phase 2 we might have:
       (gdb) call pp->m_buffer->cur_chunk_array->dump()
       0: [TEXT("foo: ")]
       1: [TEXT("42")]
       2: [TEXT(", bar: ")]
       3: [TEXT("baz")]
       4: [TEXT(", opt: ")]
       5: [BEGIN_QUOTE, TEXT("-foption"), END_QUOTE]
     For example the %qs has become the three tokens:
       [BEGIN_QUOTE, TEXT("-foption"), END_QUOTE]

     The third phase (in pp_output_formatted_text):

     (1) merges the tokens from all the chunks into one list,
     giving e.g.
      (gdb) call tokens.dump()
      [TEXT("foo: "), TEXT("42"), TEXT(", bar: "), TEXT("baz"),
       TEXT(", opt: "), BEGIN_QUOTE, TEXT("-foption"), END_QUOTE]

     (2) lowers some custom tokens into non-custom tokens

     (3) merges consecutive text tokens, giving e.g.:
      (gdb) call tokens.dump()
      [TEXT("foo: 42, bar: baz, option: "),
       BEGIN_QUOTE, TEXT("-foption"), END_QUOTE]

     (4) if provided with a urlifier, tries to apply it to quoted text,
     giving e.g:
      (gdb) call tokens.dump()
      [TEXT("foo: 42, bar: baz, option: "), BEGIN_QUOTE,
       BEGIN_URL("http://example.com"), TEXT("-foption"), END_URL, END_QUOTE]

     (5) emits all tokens in sequence with appropriate line-wrapping.  This
     can be overridded via the pretty_printer's token_printer, allowing for
     output formats to e.g. override how URLs are handled, or to handle
     custom_data that wasn't lowered in (2) above, e.g. for handling JSON
     output of optimization records.  */
  pp_token_list *m_args[PP_NL_ARGMAX * 2];

  /* The pp_tokens, pp_token_lists, and the accumulated text buffers are
     allocated within the output_buffer's chunk_obstack.  In the above
     example, the in-memory layout of the chunk_obstack might look like
     this after phase 1:

      + pp_formatted_chunks instance   <--- START of pp_formatted_chunks level
      |
      + pp_token_list for chunk 0 (m_first: *)
      |                                     |
      + "foo: \0"  <-------------\          |
      |                          |          |
      + pp_token_text (borrowed: *) <-------/
      |
      + pp_token_list for chunk 1
      |
      + "i\0" <------------------\
      |                          |
      + pp_token_text (borrowed: *)
      |
      +  ...etc for chunks 2 to 4...
      |
      + pp_token_list for chunk 5
      |
      + "qs\0" <-----------------\
      |                          |
      + pp_token_text (borrowed: *)
      |
      |
      V
     obstack grows this way

     At each stage, allocation of additional text buffers, tokens, and lists
     grow forwards in the obstack (though the internal pointers in linked
     lists might point backwards to earlier objects within the same
     pp_formatted_chunks level).  */
};

#endif /* GCC_PRETTY_PRINT_FORMAT_IMPL_H */
