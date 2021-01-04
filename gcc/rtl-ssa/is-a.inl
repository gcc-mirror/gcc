// is_a<> support for RTL SSA classes                               -*- C++ -*-
// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

template<>
struct is_a_helper<rtl_ssa::def_info *>
  : static_is_a_helper<rtl_ssa::def_info *>
{
  static inline bool
  test (const rtl_ssa::access_info *ref)
  {
    return (ref->kind () == rtl_ssa::access_kind::SET
	    || ref->kind () == rtl_ssa::access_kind::PHI
	    || ref->kind () == rtl_ssa::access_kind::CLOBBER);
  }
};

template<>
struct is_a_helper<rtl_ssa::clobber_info *>
  : static_is_a_helper<rtl_ssa::clobber_info *>
{
  static inline bool
  test (const rtl_ssa::access_info *ref)
  {
    return ref->kind () == rtl_ssa::access_kind::CLOBBER;
  }
};

template<>
struct is_a_helper<rtl_ssa::use_info *>
  : static_is_a_helper<rtl_ssa::use_info *>
{
  static inline bool
  test (const rtl_ssa::access_info *ref)
  {
    return ref->kind () == rtl_ssa::access_kind::USE;
  }
};

template<>
struct is_a_helper<rtl_ssa::set_info *>
  : static_is_a_helper<rtl_ssa::set_info *>
{
  static inline bool
  test (const rtl_ssa::access_info *ref)
  {
    return (ref->kind () == rtl_ssa::access_kind::SET
	    || ref->kind () == rtl_ssa::access_kind::PHI);
  }
};

template<>
struct is_a_helper<rtl_ssa::phi_info *>
  : static_is_a_helper<rtl_ssa::phi_info *>
{
  static inline bool
  test (const rtl_ssa::access_info *ref)
  {
    return ref->kind () == rtl_ssa::access_kind::PHI;
  }
};

template<>
struct is_a_helper<rtl_ssa::set_node *>
  : static_is_a_helper<rtl_ssa::set_node *>
{
  static inline bool
  test (const rtl_ssa::def_node *node)
  {
    return node->contains_set ();
  }
};

template<>
struct is_a_helper<rtl_ssa::clobber_group *>
  : static_is_a_helper<rtl_ssa::clobber_group *>
{
  static inline bool
  test (const rtl_ssa::def_node *node)
  {
    return node->contains_clobber ();
  }
};
