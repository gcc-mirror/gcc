/* Pretty printers for Algol 68 front-end specific %e tags.
   Copyright (C) 2026 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef __A68_PRETTY_PRINT__
#define __A68_PRETTY_PRINT__

#include "config.h"
#include "system.h"
#include "pretty-print.h"
#include "pretty-print-format-impl.h"
#include "pretty-print-markup.h"

struct a68_format_token : public pp_element
{
public:
  struct value : public pp_token_custom_data::value
  {
    value (a68_format_token &token)
      : m_token (token)
    {
    }

    value (const value &other)
      : m_token (other.m_token)
    {
    }

    value (value &&other)
      : m_token (other.m_token)
    {
    }

    value &operator= (const value &other) = delete;
    value &operator= (value &&other) = delete;
    ~value ()
    {
    }

    void dump (FILE *out) const final override
    {
      fprintf (out, "%s", m_token.m_str);
    }

    bool as_standard_tokens (pp_token_list &out) final override
    {
      out.push_back<pp_token_text> (label_text::borrow (m_token.m_str));
      return true;
    }

    a68_format_token &m_token;
  };

  a68_format_token ()
  {
    m_str = NULL;
  }

  ~a68_format_token ()
  {
    free (m_str);
  }

  void add_to_phase_2 (pp_markup::context &ctxt) final override
  {
    auto val_ptr = std::make_unique<value> (*this);
    ctxt.m_formatted_token_list->push_back<pp_token_custom_data>
      (std::move (val_ptr));
  }

  char *m_str;
};


struct a68_moid_format_token : public a68_format_token
{
public:
  a68_moid_format_token (MOID_T *m)
  {
    m_str = xstrdup (a68_moid_to_string (m, MOID_ERROR_WIDTH, NO_NODE));
  }
};

struct a68_opmoid_format_token : public a68_format_token
{
public:
  a68_opmoid_format_token (MOID_T *m)
  {
    if (m == NO_MOID || m == M_ERROR)
      m = M_UNDEFINED;

    const char *str;
    if (m == M_VOID)
      str = (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING
	     ? "UNION (VOID, ..)"
	     : "union (void, ..)");
    else if (IS (m, SERIES_MODE))
      {
	if (PACK (m) != NO_PACK && NEXT (PACK (m)) == NO_PACK)
	  str = a68_moid_to_string (MOID (PACK (m)), MOID_ERROR_WIDTH, NO_NODE);
	else
	  str = a68_moid_to_string (m, MOID_ERROR_WIDTH, NO_NODE);
      }
    else
      str = a68_moid_to_string (m, MOID_ERROR_WIDTH, NO_NODE);

    m_str = xstrdup (str);
  }
};

struct a68_attr_format_token : public a68_format_token
{
public:
  a68_attr_format_token (enum a68_attribute a)
  {
    const char *attr_name = a68_attribute_name (a);
    gcc_assert (attr_name != NULL);
    m_str = xstrdup (attr_name);
  }
};

struct a68_construct_format_token : public a68_format_token
{
public:
  a68_construct_format_token (a68_attribute a)
  {
    do_attr (a);
  }

  a68_construct_format_token (NODE_T *p)
  {
    do_attr (ATTRIBUTE (p));
  }

private:

  void do_attr (a68_attribute a)
  {
    const char *nt = a68_attribute_name (a);
    if (nt != NO_TEXT)
      m_str = xstrdup (nt);
    else
      m_str = xstrdup ("construct");
  }
};

struct a68_symbol_format_token : public a68_format_token
{
public:
  a68_symbol_format_token (NODE_T *p)
  {
    const char *txt = NSYMBOL (p);
    char *sym = NCHAR_IN_LINE (p);
    int n = 0, size = (int) strlen (txt);

    if (txt == NO_TEXT)
      m_str = xstrdup ("symbol");
    else
      {
	if (txt[0] != sym[0] || (int) strlen (sym) < size)
	  m_str = xstrdup (txt);
	else
	  {
	    m_str = (char *) xmalloc (size + 1);
	    while (n < size)
	      {
		if (ISPRINT (sym[0]))
		  m_str[n] = sym[0];
		if (TOLOWER (txt[0]) == TOLOWER (sym[0]))
		  {
		    txt++;
		    n++;
		  }
		sym++;
	      }
	    m_str[n] = '\0';
	  }
      }
  }
};

struct a68_sort_format_token : public a68_format_token
{
public:
  a68_sort_format_token (int s)
  {
    const char *cstr;
    switch (s)
      {
      case NO_SORT: cstr = "this"; break;
      case SOFT: cstr = "a soft"; break;
      case WEAK: cstr = "a weak"; break;
      case MEEK: cstr = "a meek"; break;
      case FIRM: cstr = "a firm"; break;
      case STRONG: cstr = "a strong"; break;
      default:
	gcc_unreachable ();
      }
    m_str = xstrdup (cstr);
  }
};


struct a68_line_format_token : public a68_format_token
{
public:
  a68_line_format_token (LINE_T *l, NODE_T *n)
  {
    gcc_assert (l != NO_LINE);
    if (NUMBER (l) == 0)
      m_str = xstrdup ("in standard environment");
    else if (n != NO_NODE && NUMBER (l) == LINE_NUMBER (n))
      m_str = xstrdup ("in this line");
    else
      {
	m_str = (char *) xmalloc (18);
	if (snprintf (m_str, 18, "in line %d", NUMBER (l)) < 0)
	  gcc_unreachable ();
      }
  }
};

#endif /* ! __A68_PRETTY_PRINT__ */
