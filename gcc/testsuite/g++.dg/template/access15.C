// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Aug 2004 <nathan@codesourcery.com>
// Origin: stefaandr@hotmail.com

// Bug 17149: ICE with TEMPLATE_TYPE_PARM


template <class super,
	  int (super::tdata::*member)() const = &super::tdata::operator()>
struct x {};
