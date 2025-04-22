// PR target/119327
// { dg-do compile { target c++11 } }
// { dg-options "-Os" }

#pragma GCC optimize "fp-contract=off"

template <class T>
void
foo (T f)
{
  f ();
}

struct S {
  S () { [] {}; foo ([] __attribute__((always_inline)) {}); }
} s;
