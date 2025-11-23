// PR c++/122789
// { dg-additional-options "-fmodules -fconcepts" }

module M;
static_assert(!b<int>);
