// PR tree-optimization/92644
// { dg-do compile { target c++14 } }
// { dg-options "-O2 -fno-early-inlining" }

inline auto foo () { return nullptr; }
int bar () { return foo () ? 1 : 0; }
