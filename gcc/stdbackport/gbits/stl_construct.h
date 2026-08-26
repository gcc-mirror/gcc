// nonstandard construct and destroy functions -*- C++ -*-

// Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996,1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

#ifndef GCC_BACKPORT_STL_CONSTRUCT_H
#define GCC_BACKPORT_STL_CONSTRUCT_H 1

#include <memory>
#include <utility>

namespace gcc
{
  /**
   * Destroy the object pointed to by a pointer type.
   */
  template<typename G_Tp>
    constexpr inline std::enable_if_t<!std::is_array<G_Tp>::value>
    destroy_at(G_Tp* g_pointer)
    {
      g_pointer->~G_Tp();
    }

  template <typename G_Tp>
  inline std::enable_if_t<std::is_array<G_Tp>::value>
    destroy_at(G_Tp* __location)
    {
      for (auto& __x : *__location)
	gcc::destroy_at(std::addressof(__x));
    }

#if __cplusplus < 202002L
  namespace detail {
    template<typename G_Up, typename... G_Args>
      static decltype (::new((void*)0) G_Up(std::declval<G_Args>()...),
		       std::true_type {})
      placement_new_test (int);

    template<typename G_Up, typename... G_Args>
      static std::false_type
      placement_new_test (...);
  } // namespace detail

  template<typename G_Tp, typename... G_Args>
    using can_placement_new
      = decltype(detail::placement_new_test<G_Tp, G_Args...>(0));

  template<typename G_Tp, typename... G_Args>
  using can_use_construct_at = and_<can_placement_new<G_Tp, G_Args...>,
				    not_<is_unbounded_array<G_Tp>>>;

  template<typename G_Tp, typename... G_Args>
    constexpr std::enable_if_t<and_<can_use_construct_at<G_Tp, G_Args...>,
				    std::is_array<G_Tp>>::value,
			       G_Tp*>
  construct_at (G_Tp* g_location, G_Args&&...)
  {
    void* g_loc = g_location;
    static_assert(sizeof...(G_Args) == 0, "std::construct_at for array "
		  "types must not use any arguments to initialize the "
		  "array");
    return ::new(g_loc) G_Tp[1]();
  }
  template<typename G_Tp, typename... G_Args>
    constexpr std::enable_if_t<and_<can_use_construct_at<G_Tp, G_Args...>,
				    not_<std::is_array<G_Tp>>>::value,
			       G_Tp*>
  construct_at (G_Tp* g_location, G_Args&&... g_args)
  {
    void* g_loc = g_location;
    return ::new(g_loc) G_Tp(std::forward<G_Args>(g_args)...);
  }
#else // C++ >= 20
  // Use standard instead of our own implementation, to permit constexpr use.
  // Useful for running testsuites against expected.
  using std::construct_at;
#endif

} // namespace gcc

#endif /* GCC_BACKPORT_STL_CONSTRUCT_H */
