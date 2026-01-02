/* Template for deferring object creation until the object is needed.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_LAZILY_CREATED_H
#define GCC_LAZILY_CREATED_H

/* A template for deferring object creation for a type T until the object
   is needed, to avoid potentially expensive creation of T unless it's
   actually needed (e.g. for directed graphs associated with a diagnostic,
   which are ignored by the default "text" sink).  */

template <typename T>
class lazily_created
{
 public:
  virtual ~lazily_created () {}

  const T &
  get_or_create () const
  {
    if (!m_object)
      m_object = create_object ();
    gcc_assert (m_object);
    return *m_object;
  }

private:
  virtual std::unique_ptr<T>
  create_object () const = 0;

  mutable std::unique_ptr<T> m_object;
};

#endif /* ! GCC_LAZILY_CREATED_H */
