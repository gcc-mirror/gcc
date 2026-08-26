// Tag types for inplace construction. -*- C++ -*-

// Copyright (C) 2004-2026 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef GCC_BACKPORT_GLIBCXX_INPLACE_TAGS_H
#define GCC_BACKPORT_GLIBCXX_INPLACE_TAGS_H 1

#if __cplusplus >= 201703L
# include <utility>
#endif
#include "extras.h"

namespace gcc
{
#if __cplusplus >= 201703L // C++17
  using std::in_place_t;
  using std::in_place;
#else // !C++17
  struct in_place_t {
    explicit in_place_t() = default;
  };

  static constexpr in_place_t in_place{};
#endif // !C++17

  template<typename T>
  using is_in_place = std::is_same<remove_cvref_t<T>, in_place_t>;
} // namespace gcc

#endif /* GCC_BACKPORT_GLIBCXX_INPLACE_TAGS_H */
