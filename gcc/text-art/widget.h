/* Hierarchical diagram elements.
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

#ifndef GCC_TEXT_ART_WIDGET_H
#define GCC_TEXT_ART_WIDGET_H

#include "text-art/canvas.h"
#include "text-art/table.h"

namespace text_art {

/* Abstract base class: something that knows how to size itself and
   how to paint itself to a canvas, potentially with children, with
   support for hierarchical sizing and positioning.

   Widgets have a two-phase sizing/positioning algorithm.

   Step 1: size requests: the root widget is asked for its size request i.e
   how big it wants to be.  This is handled by recursively asking child
   widgets for their requested sizes.  Each widget subclass can implement
   their own logic for this in the "calc_req_size" vfunc, and the result
   is cached in m_req_size.

   Step 2: rect allocation: the root widget is set a canvas::rect_t as
   its "allocated" rectangle.  Each widget subclass can then place its
   children recursively using the "update_child_alloc_rects" vfunc.
   For simplicity, all coordinates in the hierarchy are within the same
   coordinate system (rather than attempting to store per-child offsets).

   Widget subclasses are responsible for managing their own children.  */

/* Subclasses in this header, with indentation indicating inheritance.  */

class widget;  /* Abstract base class.  */
  class wrapper_widget;  /* Concrete subclass: a widget with a single child.  */
  class container_widget; /* Abstract subclass: widgets with an arbitrary
			     number of children.  */
    class vbox_widget; /* Concrete widget subclass: lay out children
			  vertically.  */
  class leaf_widget; /* Abstract subclass: a widget with no children.  */
    class text_widget; /* Concrete subclass: a text string.  */
    class canvas_widget; /* Concrete subclass: a pre-rendered canvas.  */

class widget
{
 public:
  /* This can be very useful for debugging when implementing new
     widget subclasses.  */
  static const bool DEBUG_GEOMETRY = false;

  virtual ~widget () {}

  canvas to_canvas (const style_manager &style_mgr);

  canvas::size_t get_req_size ()
  {
    m_req_size = calc_req_size();
    if (DEBUG_GEOMETRY)
      fprintf (stderr, "calc_req_size (%s) -> (w:%i, h:%i)\n",
	       get_desc (),
	       m_req_size.w, m_req_size.h);
    return m_req_size;
  }

  void set_alloc_rect (const canvas::rect_t &rect)
  {
    if (DEBUG_GEOMETRY)
      fprintf (stderr, "set_alloc_rect (%s): ((x:%i, y:%i), (w:%i, h:%i))\n",
	       get_desc (),
	       rect.m_top_left.x, rect.m_top_left.y,
	       rect.m_size.w, rect.m_size.h);
    m_alloc_rect = rect;
    update_child_alloc_rects ();
  }

  virtual const char *get_desc () const = 0;
  virtual canvas::size_t calc_req_size () = 0;
  virtual void update_child_alloc_rects () = 0;
  virtual void paint_to_canvas (canvas &canvas) = 0;

  /* Access to the cached size request of this widget.  */
  const canvas::size_t get_req_size () const { return m_req_size; }
  int get_req_w () const { return m_req_size.w; }
  int get_req_h () const { return m_req_size.h; }

  /* Access to the allocated canvas coordinates of this widget.  */
  const canvas::rect_t &get_alloc_rect () const { return m_alloc_rect; }
  int get_alloc_w () const { return m_alloc_rect.get_width (); }
  int get_alloc_h () const { return m_alloc_rect.get_height (); }
  int get_min_x () const { return m_alloc_rect.get_min_x (); }
  int get_max_x () const { return m_alloc_rect.get_max_x (); }
  int get_next_x () const { return m_alloc_rect.get_next_x (); }
  int get_min_y () const { return m_alloc_rect.get_min_y (); }
  int get_max_y () const { return m_alloc_rect.get_max_y (); }
  int get_next_y () const { return m_alloc_rect.get_max_y (); }
  canvas::range_t get_x_range () const { return m_alloc_rect.get_x_range (); }
  canvas::range_t get_y_range () const { return m_alloc_rect.get_y_range (); }
  const canvas::coord_t &get_top_left () const
  {
    return m_alloc_rect.m_top_left;
  }

 protected:
  widget ()
  : m_req_size (0, 0),
    m_alloc_rect (canvas::coord_t (0, 0),
		  canvas::size_t (0, 0))
  {}

private:
  /* How much size this widget requested.  */
  canvas::size_t m_req_size;
  /* Where (and how big) this widget was allocated.  */
  canvas::rect_t m_alloc_rect;
};

/* Concrete subclass for a widget with a single child.  */

class wrapper_widget : public widget
{
 public:
  wrapper_widget (std::unique_ptr<widget> child)
  : m_child (std::move (child))
  {}

  const char *get_desc () const override
  {
    return "wrapper_widget";
  }
  canvas::size_t calc_req_size () override
  {
    return m_child->get_req_size ();
  }
  void update_child_alloc_rects () override
  {
    m_child->set_alloc_rect (get_alloc_rect ());
  }
  void paint_to_canvas (canvas &canvas) override
  {
    m_child->paint_to_canvas (canvas);
  }
 private:
  std::unique_ptr<widget> m_child;
};

/* Abstract subclass for widgets with an arbitrary number of children.  */

class container_widget : public widget
{
 public:
  void add_child (std::unique_ptr<widget> child)
  {
    m_children.push_back (std::move (child));
  }

  void paint_to_canvas (canvas &canvas) final override
  {
    for (auto &child : m_children)
      child->paint_to_canvas (canvas);
  }

 protected:
  std::vector<std::unique_ptr<widget>> m_children;
};

/* Concrete widget subclass: lay out children vertically.  */

class vbox_widget : public container_widget
{
 public:
  const char *get_desc () const override;
  canvas::size_t calc_req_size () override;
  void update_child_alloc_rects () final override;
};

/* Abstract subclass for widgets with no children.  */

class leaf_widget : public widget
{
 public:
  void update_child_alloc_rects () final override
  {
    /* no-op.  */
  }

 protected:
  leaf_widget () : widget () {}
};

/* Concrete widget subclass for a text string.  */

class text_widget : public leaf_widget
{
 public:
  text_widget (styled_string str)
  : leaf_widget (), m_str (std::move (str))
  {
  }

  const char *get_desc () const override;
  canvas::size_t calc_req_size () final override;
  void paint_to_canvas (canvas &canvas) final override;

private:
  styled_string m_str;
};

/* Concrete widget subclass for a pre-rendered canvas.  */

class canvas_widget : public leaf_widget
{
 public:
  canvas_widget (canvas &&c)
  : leaf_widget (), m_canvas (std::move (c))
  {
  }

  const char *get_desc () const override;
  canvas::size_t calc_req_size () final override;
  void paint_to_canvas (canvas &canvas) final override;

private:
  canvas m_canvas;
};

} // namespace text_art

#endif /* GCC_TEXT_ART_WIDGET_H */
