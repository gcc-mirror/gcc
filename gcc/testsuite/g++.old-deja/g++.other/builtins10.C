// { dg-do assemble  }
// Test that built-in functions do warn when prototyped without arguments.
// Origin: PR c++/9367
// Copyright (C) 2003 Free Software Foundation.

extern "C" int snprintf(); // { dg-warning "conflicts with built-in declaration" "" { target c++11 } }
extern "C" int printf(); // { dg-warning "conflicts with built-in declaration" }

