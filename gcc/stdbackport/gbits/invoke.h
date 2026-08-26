// Implementation of INVOKE -*- C++ -*-

// Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

#ifndef GCC_BACKPORT_GLIBCXX_INVOKE_H
#define GCC_BACKPORT_GLIBCXX_INVOKE_H 1

#include <type_traits>
#include <utility>

namespace gcc
{
  // Used by __invoke_impl instead of std::forward<_Tp> so that a
  // reference_wrapper is converted to an lvalue-reference.
  template<typename G_Tp, typename G_Up = typename g_inv_unwrap<G_Tp>::type>
    constexpr G_Up&&
    g_invfwd(typename std::remove_reference<G_Tp>::type& g_t) noexcept
    { return static_cast<G_Up&&>(g_t); }

  template<typename G_Res, typename G_Fn, typename... G_Args>
    constexpr G_Res
    g_invoke_impl(g_invoke_other, G_Fn&& g_f, G_Args&&... g_args)
    { return std::forward<G_Fn>(g_f)(std::forward<G_Args>(g_args)...); }

  template<typename G_Res, typename G_MemFun, typename G_Tp, typename... G_Args>
    constexpr G_Res
    g_invoke_impl(g_invoke_memfun_ref, G_MemFun&& g_f, G_Tp&& g_t,
		  G_Args&&... g_args)
    { return (g_invfwd<G_Tp>(g_t).*g_f)(std::forward<G_Args>(g_args)...); }

  template<typename G_Res, typename G_MemFun, typename G_Tp, typename... G_Args>
    constexpr G_Res
    g_invoke_impl(g_invoke_memfun_deref, G_MemFun&& g_f, G_Tp&& g_t,
		  G_Args&&... g_args)
    {
      return ((*std::forward<G_Tp>(g_t)).*g_f)(std::forward<G_Args>(g_args)...);
    }

  template<typename G_Res, typename G_MemPtr, typename G_Tp>
    constexpr G_Res
    g_invoke_impl(g_invoke_memobj_ref, G_MemPtr&& g_f, G_Tp&& g_t)
    { return g_invfwd<G_Tp>(g_t).*g_f; }

  template<typename G_Res, typename G_MemPtr, typename G_Tp>
    constexpr G_Res
    g_invoke_impl(g_invoke_memobj_deref, G_MemPtr&& g_f, G_Tp&& g_t)
    { return (*std::forward<G_Tp>(g_t)).*g_f; }

  /// Invoke a callable object.
  template<typename G_Callable, typename... G_Args>
    constexpr typename g_invoke_result<G_Callable, G_Args...>::type
    invoke(G_Callable&& g_fn, G_Args&&... g_args)
    noexcept(g_is_nothrow_invocable<G_Callable, G_Args...>::value)
    {
      using g_result = g_invoke_result<G_Callable, G_Args...>;
      using g_type = typename g_result::type;
      using g_tag = typename g_result::g_invoke_type;
      return gcc::g_invoke_impl<g_type>(g_tag{}, std::forward<G_Callable>(g_fn),
					std::forward<G_Args>(g_args)...);
    }
} // namespace gcc

#endif // GCC_BACKPORT_GLIBCXX_INVOKE_H
