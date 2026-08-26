// <bits/enable_special_members.h> -*- C++ -*-

// Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

#ifndef GCC_BACKPORT_ENABLE_SPECIAL_MEMBERS_H
#define GCC_BACKPORT_ENABLE_SPECIAL_MEMBERS_H 1

namespace gcc
{
  struct G_Enable_default_constructor_tag
  {
    explicit constexpr G_Enable_default_constructor_tag() = default;
  };

/**
  * @brief A mixin helper to conditionally enable or disable the default
  * constructor.
  * @sa _Enable_special_members
  */
template<bool G_Switch, typename G_Tag = void>
  struct G_Enable_default_constructor
  {
    constexpr G_Enable_default_constructor() noexcept = default;
    constexpr G_Enable_default_constructor(G_Enable_default_constructor const&)
      noexcept  = default;
    constexpr G_Enable_default_constructor(G_Enable_default_constructor&&)
      noexcept = default;
    G_Enable_default_constructor&
    operator=(G_Enable_default_constructor const&) noexcept = default;
    G_Enable_default_constructor&
    operator=(G_Enable_default_constructor&&) noexcept = default;

    // Can be used in other ctors.
    constexpr explicit
    G_Enable_default_constructor(G_Enable_default_constructor_tag) { }
  };


/**
  * @brief A mixin helper to conditionally enable or disable the default
  * destructor.
  * @sa _Enable_special_members
  */
template<bool G_Switch, typename G_Tag = void>
  struct G_Enable_destructor { };

/**
  * @brief A mixin helper to conditionally enable or disable the copy/move
  * special members.
  * @sa _Enable_special_members
  */
template<bool G_Copy, bool G_CopyAssignment,
         bool G_Move, bool G_MoveAssignment,
         typename G_Tag = void>
  struct G_Enable_copy_move { };

/**
  * @brief A mixin helper to conditionally enable or disable the special
  * members.
  *
  * The @c _Tag type parameter is to make mixin bases unique and thus avoid
  * ambiguities.
  */
template<bool G_Default, bool G_Destructor,
         bool G_Copy, bool G_CopyAssignment,
         bool G_Move, bool G_MoveAssignment,
         typename G_Tag = void>
  struct G_Enable_special_members
  : private G_Enable_default_constructor<G_Default, G_Tag>,
    private G_Enable_destructor<G_Destructor, G_Tag>,
    private G_Enable_copy_move<G_Copy, G_CopyAssignment,
                              G_Move, G_MoveAssignment,
                              G_Tag>
  { };

// Boilerplate follows.

template<typename G_Tag>
  struct G_Enable_default_constructor<false, G_Tag>
  {
    constexpr G_Enable_default_constructor() noexcept = delete;
    constexpr G_Enable_default_constructor(G_Enable_default_constructor const&)
      noexcept  = default;
    constexpr G_Enable_default_constructor(G_Enable_default_constructor&&)
      noexcept = default;
    G_Enable_default_constructor&
    operator=(G_Enable_default_constructor const&) noexcept = default;
    G_Enable_default_constructor&
    operator=(G_Enable_default_constructor&&) noexcept = default;

    // Can be used in other ctors.
    constexpr explicit
    G_Enable_default_constructor(G_Enable_default_constructor_tag) { }
  };

template<typename G_Tag>
  struct G_Enable_destructor<false, G_Tag>
  { ~G_Enable_destructor() noexcept = delete; };

template<typename G_Tag>
  struct G_Enable_copy_move<false, true, true, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, false, true, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, false, true, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, true, false, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, true, false, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, false, false, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, false, false, true, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = default;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, true, true, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, true, true, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, false, true, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, false, true, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, true, false, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, true, false, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = default;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<true, false, false, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

template<typename G_Tag>
  struct G_Enable_copy_move<false, false, false, false, G_Tag>
  {
    constexpr G_Enable_copy_move() noexcept                          = default;
    constexpr G_Enable_copy_move(G_Enable_copy_move const&) noexcept  = delete;
    constexpr G_Enable_copy_move(G_Enable_copy_move&&) noexcept       = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move const&) noexcept                    = delete;
    G_Enable_copy_move&
    operator=(G_Enable_copy_move&&) noexcept                         = delete;
  };

} // namespace gcc

#endif // GCC_BACKPORT_ENABLE_SPECIAL_MEMBERS_H
