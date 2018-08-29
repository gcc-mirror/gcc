// PR c++/84444
// { dg-do compile }
// { dg-options "-O2" }

struct A {};

__UINTPTR_TYPE__
foo (A *p)
{
  return (__UINTPTR_TYPE__) __builtin_launder (p);
}
