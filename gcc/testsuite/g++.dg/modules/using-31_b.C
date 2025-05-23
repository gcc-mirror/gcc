// PR c++/120414
// { dg-additional-options "-fmodules" }

module m;
static_assert(Derived<1>::go() == 1);
