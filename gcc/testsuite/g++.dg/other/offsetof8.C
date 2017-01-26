// PR c++/68711 - [5 regression] SEGV on an invalid offsetof of a member
//                of a virtual base
// { dg-do compile }

struct A { int i; };

struct B: virtual A { };

int a[]  = {
  !&((B*)0)->i,    // { dg-error "invalid access to non-static data member" }
  __builtin_offsetof (B, i)   // { dg-error "invalid access to non-static" }
};			      // { dg-error "offsetof within non-standard-layout type" "" { target *-*-* } .-1 }
