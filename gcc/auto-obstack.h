/* RAII wrapper around obstack.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
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

#ifndef GCC_AUTO_OBSTACK_H
#define GCC_AUTO_OBSTACK_H

/* RAII wrapper around obstack.  */

struct auto_obstack
{
  auto_obstack ()
  {
    obstack_init (&m_obstack);
  }

  ~auto_obstack ()
  {
    obstack_free (&m_obstack, NULL);
  }

  operator obstack & () { return m_obstack; }

  void grow (const void *src, size_t length)
  {
    obstack_grow (&m_obstack, src, length);
  }

  void *object_base () const
  {
    return m_obstack.object_base;
  }

  size_t object_size () const
  {
    return obstack_object_size (&m_obstack);
  }

  obstack m_obstack;
};

#endif /* GCC_AUTO_OBSTACK_H */
