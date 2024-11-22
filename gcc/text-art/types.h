/* Types for drawing 2d "text art".
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_TEXT_ART_TYPES_H
#define GCC_TEXT_ART_TYPES_H

/* This header uses std::vector, but <vector> can't be directly
   included due to issues with macros.  Hence it must be included from
   system.h by defining INCLUDE_VECTOR in any source file using it.  */

#ifndef INCLUDE_VECTOR
# error "You must define INCLUDE_VECTOR before including system.h to use text-art/types.h"
#endif

#include "cpplib.h"
#include "pretty-print.h"

namespace text_art {

/* Forward decls.  */

class canvas;
class table;
class theme;

/* Classes for geometry.
   We use templates to avoid mixing up e.g. canvas coordinates
   with table coordinates.  */

template <typename CoordinateSystem>
struct size
{
  size (int w_, int h_) : w (w_), h (h_) {}
  int w;
  int h;
};

template <typename CoordinateSystem>
struct coord
{
  coord (int x_, int y_) : x (x_), y (y_) {}
  int x;
  int y;
};

template <typename CoordinateSystem>
coord<CoordinateSystem> operator+ (coord<CoordinateSystem> a,
				   coord<CoordinateSystem> b)
{
  return coord<CoordinateSystem> (a.x + b.x, a.y + b.y);
}

/* A half-open range [start, next) of int.  */

template <typename CoordinateSystem>
struct range
{
  range (int start_, int next_)
  : start (start_), next (next_)
  {}

  int get_min () const { return start; }
  int get_max () const { return next - 1; }
  int get_next () const { return next; }
  int get_size () const { return next - start; }

  int get_midpoint () const { return get_min () + get_size () / 2; }

  int start;
  int next;
};

/* A rectangle area within CoordinateSystem.  */

template <typename CoordinateSystem>
struct rect
{
  rect (coord<CoordinateSystem> top_left,
	size<CoordinateSystem> size)
  : m_top_left (top_left),
    m_size (size)
  {
  }

  rect (range<CoordinateSystem> x_range,
	range<CoordinateSystem> y_range)
  : m_top_left (x_range.get_min (), y_range.get_min ()),
    m_size (x_range.get_size (), y_range.get_size ())
  {
  }

  int get_min_x () const { return m_top_left.x; }
  int get_min_y () const { return m_top_left.y; }
  int get_max_x () const { return m_top_left.x + m_size.w - 1; }
  int get_max_y () const { return m_top_left.y + m_size.h - 1; }
  int get_next_x () const { return m_top_left.x + m_size.w; }
  int get_next_y () const { return m_top_left.y + m_size.h; }

  range<CoordinateSystem> get_x_range () const
  {
    return range<CoordinateSystem> (get_min_x (), get_next_x ());
  }
  range<CoordinateSystem> get_y_range () const
  {
    return range<CoordinateSystem> (get_min_y (), get_next_y ());
  }

  int get_width () const { return m_size.w; }
  int get_height () const { return m_size.h; }

  coord<CoordinateSystem> m_top_left;
  size<CoordinateSystem> m_size;
};

template <typename CoordinateSystem>
rect<CoordinateSystem> operator+ (rect<CoordinateSystem> r,
				  coord<CoordinateSystem> offset)
{
  return rect<CoordinateSystem> (r.m_top_left + offset, r.m_size);
}

template <typename ElementType, typename SizeType, typename CoordType>
class array2
{
 public:
  typedef ElementType element_t;
  typedef SizeType size_t;
  typedef CoordType coord_t;

  array2 (size_t sz)
  : m_size (sz),
    m_elements (sz.w * sz.h)
  {
  }
  array2 (array2 &&other)
  : m_size (other.m_size),
    m_elements (std::move (other.m_elements))
  {
  }

  /* Move assignment not implemented or used.  */
  array2 &operator== (array2 &&other) = delete;

  /* No copy ctor or assignment op.  */
  array2 (const array2 &other) = delete;
  array2 &operator= (const array2 &other) = delete;


  const size_t &get_size () const { return m_size; }

  void add_row (const element_t &element)
  {
    m_size.h++;
    m_elements.insert (m_elements.end (), m_size.w, element);
  }

  const element_t &get (const coord_t &coord) const
  {
    ::size_t idx = get_idx (coord);
    return m_elements[idx];
  }

  void set (const coord_t &coord, const element_t &element)
  {
    ::size_t idx = get_idx (coord);
    m_elements[idx] = element;
  }

  void fill (element_t element)
  {
    for (int y = 0; y < m_size.h; y++)
      for (int x = 0; x < m_size.w; x++)
	set (coord_t (x, y), element);
  }

 private:
  ::size_t get_idx (const coord_t &coord) const
  {
    gcc_assert (coord.x >= 0);
    gcc_assert (coord.x < m_size.w);
    gcc_assert (coord.y >= 0);
    gcc_assert (coord.y < m_size.h);
    return (coord.y * m_size.w) + coord.x;
  }

  size_t m_size;
  std::vector<element_t> m_elements;
};

/* A combination of attributes describing how to style a text cell.
   We only support those attributes mentioned in invoke.texi:
   - bold,
   - underscore,
   - blink,
   - inverse,
   - colors for foreground and background:
     - default color
     - named colors
     - 16-color mode colors (the "bright" variants)
     - 88-color mode
     - 256-color mode
   plus URLs. */

struct style
{
  typedef unsigned char id_t;
  static const id_t id_plain = 0;

  /* Colors.  */
  enum class named_color
  {
   DEFAULT,
   // ANSI order
   BLACK,
   RED,
   GREEN,
   YELLOW,
   BLUE,
   MAGENTA,
   CYAN,
   WHITE
  };


  struct color
  {
    enum class kind
    {
      NAMED,
      BITS_8,
      BITS_24,
    } m_kind;

    union
    {
      struct {
	enum named_color m_name;
	bool m_bright;
      } m_named;
      uint8_t m_8bit;
      struct {
	uint8_t r;
	uint8_t g;
	uint8_t b;
      } m_24bit;
    } u;

    /* Constructor for named colors.  */
    color (enum named_color name = named_color::DEFAULT,
	   bool bright = false)
    : m_kind (kind::NAMED)
    {
      u.m_named.m_name = name;
      u.m_named.m_bright = bright;
    }

    /* Constructor for 8-bit colors.  */
    color (uint8_t col_val)
    : m_kind (kind::BITS_8)
    {
      u.m_8bit = col_val;
    }

    /* Constructor for 24-bit colors.  */
    color (uint8_t r, uint8_t g, uint8_t b)
    : m_kind (kind::BITS_24)
    {
      u.m_24bit.r = r;
      u.m_24bit.g = g;
      u.m_24bit.b = b;
    }

    bool operator== (const color &other) const;
    bool operator!= (const color &other) const
    {
      return !(*this == other);
    }

    void print_sgr (pretty_printer *pp, bool fg, bool &need_separator) const;
  };

  style ()
  : m_bold (false),
    m_underscore (false),
    m_blink (false),
    m_reverse (false),
    m_fg_color (named_color::DEFAULT),
    m_bg_color (named_color::DEFAULT),
    m_url ()
  {}

  bool operator== (const style &other) const
  {
    return (m_bold == other.m_bold
	    && m_underscore == other.m_underscore
	    && m_blink == other.m_blink
	    && m_reverse == other.m_reverse
	    && m_fg_color == other.m_fg_color
	    && m_bg_color == other.m_bg_color
	    && m_url == other.m_url);
  }

  style &set_style_url (const char *url);

  static void print_changes (pretty_printer *pp,
			     const style &old_style,
			     const style &new_style);

  bool m_bold;
  bool m_underscore;
  bool m_blink;
  bool m_reverse;
  color m_fg_color;
  color m_bg_color;
  std::vector<cppchar_t> m_url; // empty = no URL
};

extern style get_style_from_color_cap_name (const char *name);

/* A class to keep track of all the styles in use in a drawing, so that
   we can refer to them via the compact style::id_t type, rather than
   via e.g. pointers.  */

class style_manager
{
 public:
  style_manager ();
  style::id_t get_or_create_id (const style &style);
  const style &get_style (style::id_t id) const
  {
    return m_styles[id];
  }
  void print_any_style_changes (pretty_printer *pp,
				style::id_t old_id,
				style::id_t new_id) const;
  unsigned get_num_styles () const { return m_styles.size (); }

private:
  std::vector<style> m_styles;
};

class styled_unichar
{
 public:
  friend class styled_string;

  explicit styled_unichar ()
  : m_code (0),
    m_style_id (style::id_plain)
  {}
  explicit styled_unichar (cppchar_t ch)
  : m_code (ch),
    m_emoji_variant_p (false),
    m_style_id (style::id_plain)
  {}
  explicit styled_unichar (cppchar_t ch, bool emoji, style::id_t style_id)
  : m_code (ch),
    m_emoji_variant_p (emoji),
    m_style_id (style_id)
  {
    gcc_assert (style_id <= 0x7f);
  }

  cppchar_t get_code () const { return m_code; }
  bool emoji_variant_p () const { return m_emoji_variant_p; }
  style::id_t get_style_id () const { return m_style_id; }

  bool double_width_p () const
  {
    int width = cpp_wcwidth (get_code ());
    gcc_assert (width == 1 || width == 2);
    return width == 2;
  }

  bool operator== (const styled_unichar &other) const
  {
    return (m_code == other.m_code
	    && m_emoji_variant_p == other.m_emoji_variant_p
	    && m_style_id == other.m_style_id);
  }

  void set_emoji_variant () { m_emoji_variant_p = true; }

  int get_canvas_width () const
  {
      return cpp_wcwidth (m_code);
  }

  void add_combining_char (cppchar_t ch)
  {
    m_combining_chars.push_back (ch);
  }

  const std::vector<cppchar_t> get_combining_chars () const
  {
    return m_combining_chars;
  }

private:
  cppchar_t m_code : 24;
  bool m_emoji_variant_p : 1;
  style::id_t m_style_id : 7;
  std::vector<cppchar_t> m_combining_chars;
};

class styled_string
{
 public:
  explicit styled_string () = default;
  explicit styled_string (style_manager &sm, const char *str);
  explicit styled_string (cppchar_t cppchar, bool emoji = false);

  styled_string (styled_string &&) = default;
  styled_string &operator= (styled_string &&) = default;

  /* No copy ctor or assignment op.  */
  styled_string (const styled_string &) = delete;
  styled_string &operator= (const styled_string &) = delete;

  /* For the few cases where copying is required, spell it out explicitly.  */
  styled_string copy () const
  {
    styled_string result;
    result.m_chars = m_chars;
    return result;
  }

  bool operator== (const styled_string &other) const
  {
    return m_chars == other.m_chars;
  }

  static styled_string from_fmt (style_manager &sm,
				 printer_fn format_decoder,
				 const char *fmt, ...)
    ATTRIBUTE_GCC_PPDIAG(3, 4);
  static styled_string from_fmt_va (style_manager &sm,
				    printer_fn format_decoder,
				    const char *fmt,
				    va_list *args)
    ATTRIBUTE_GCC_PPDIAG(3, 0);

  size_t size () const { return m_chars.size (); }
  styled_unichar operator[] (size_t idx) const { return m_chars[idx]; }

  std::vector<styled_unichar>::const_iterator begin () const
  {
    return m_chars.begin ();
  }
  std::vector<styled_unichar>::const_iterator end () const
  {
    return m_chars.end ();
  }

  int calc_canvas_width () const;

  void append (const styled_string &suffix);

  void set_url (style_manager &sm, const char *url);

private:
  std::vector<styled_unichar> m_chars;
};

enum class x_align
{
  LEFT,
  CENTER,
  RIGHT
};

enum class y_align
{
  TOP,
  CENTER,
  BOTTOM
};

/* A set of cardinal directions within a canvas or table.  */

struct directions
{
public:
  directions (bool up, bool down, bool left, bool right)
  : m_up (up), m_down (down), m_left (left), m_right (right)
  {
  }

  size_t as_index () const
  {
    return (m_up << 3) | (m_down << 2) | (m_left << 1) | m_right;
  }

  bool m_up: 1;
  bool m_down: 1;
  bool m_left: 1;
  bool m_right: 1;
};

} // namespace text_art

#endif /* GCC_TEXT_ART_TYPES_H */
