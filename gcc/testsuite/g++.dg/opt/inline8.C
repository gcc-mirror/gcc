// PR c++/15871
// { dg-options "-O2 -fkeep-inline-functions" }
// { dg-final { scan-assembler "foo" } }

inline void foo(void) { }
