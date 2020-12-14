// Iterator-related utilities.
// Copyright (C) 2002-2020 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef GCC_ITERATOR_UTILS_H
#define GCC_ITERATOR_UTILS_H 1

// A half-open [begin, end) range of iterators.
template<typename T>
struct iterator_range
{
public:
  using const_iterator = T;

  iterator_range () = default;
  iterator_range (const T &begin, const T &end)
    : m_begin (begin), m_end (end) {}

  T begin () const { return m_begin; }
  T end () const { return m_end; }

  explicit operator bool () const { return m_begin != m_end; }

private:
  T m_begin;
  T m_end;
};

#endif
