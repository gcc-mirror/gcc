// PR c++/69912 - [6 regression] ICE in build_ctor_subob_ref initializing
//                a flexible array member
// { dg-do compile }
// { dg-options "-Wno-pedantic -Wno-write-strings -fpermissive" }

struct S {
  int n; 
  char *a[];
};

void foo (const char *a)
{
  const S s = { 1, { a, "b" } };   // { dg-warning "invalid conversion" }
}
