// functional_hash.h header but gutted -*- C++ -*-

// Copyright (C) 2007-2026 Free Software Foundation, Inc.
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

#ifndef GCC_BACKPORT_FUNCTIONAL_HASH_H
#define GCC_BACKPORT_FUNCTIONAL_HASH_H 1

#define WANT_FUNCTIONAL
#include "check_system_h.h"

#include <type_traits>
#include "extras.h"

namespace gcc
{
  template<typename G_Tp, typename = void>
    struct g_is_hash_enabled_for : std::false_type {};

  template<typename G_Tp>
    struct
    g_is_hash_enabled_for<G_Tp,
			  void_t<decltype(std::hash<G_Tp>()(std::declval<G_Tp>()))>>
    : std::true_type {};

  // Helper struct for defining disabled specializations of std::hash.
  template<typename G_Tp>
    struct g_hash_not_enabled
    {
      g_hash_not_enabled(g_hash_not_enabled&&) = delete;
      ~g_hash_not_enabled() = delete;
    };
} // namespace

#endif // GCC_BACKPORT_FUNCTIONAL_HASH_H
