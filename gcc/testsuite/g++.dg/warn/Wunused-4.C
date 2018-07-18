// { dg-do compile }
// { dg-options "-Wunused-parameter" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Sep 2003 <nathan@codesourcery.com>
// Origin: yotamm@mellanox.co.il


// PR c++/9848. Missing warning

struct C1 {
  // Only use in-charge ctor
  C1(int bi) {}  // { dg-warning "unused parameter" }
};
struct C2 {
  // Only use base ctor
  C2(int bi) {}  // { dg-warning "unused parameter" }
};

struct D : C2
{
  D (int) : C2 (1) {}
};

void show_compile_warning ()
{
  C1 c1 (1);
  
  D d (1);
}
