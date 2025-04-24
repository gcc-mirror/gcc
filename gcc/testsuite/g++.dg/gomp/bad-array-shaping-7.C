// { dg-do compile }
// { dg-additional-options "-std=c++11" }

#include <new>

template<typename T>
struct St {
  T *pp;
};

template<typename T>
void foo (T *w)
{
  alignas (St<T>) unsigned char buf[sizeof (St<T>)];
  T *sub1;

  /* No array shaping op in brace initialiser (nonsensical anyway, but make
     sure it doesn't parse).  */
#pragma omp target update to( new (buf) St<T> { ([10][10]) sub1 } )
// { dg-error {expected identifier before numeric constant} "" { target *-*-* } .-1 }
// { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-2 }
// { dg-error {expected '\}' before 'sub1'} "" { target *-*-* } .-3 }
}

struct S {
  int *pp;
};

int main()
{
  alignas (S) unsigned char buf[sizeof (S)];
  int *sub1;

  // As above.
#pragma omp target update to( new (buf) S { ([10][10]) sub1 } )
// { dg-error {expected identifier before numeric constant} "" { target *-*-* } .-1 }
// { dg-error {expected '\{' before '\[' token} "" { target *-*-* } .-2 }
// { dg-error {expected '\}' before 'sub1'} "" { target *-*-* } .-3 }
// { dg-error {no match for 'operator\[\]'} "" { target *-*-* } .-4 }
// { dg-error {could not convert} "" { target *-*-* } .-5 }
// { dg-error {'#pragma omp target update' must contain at least one 'from' or 'to' clauses} "" { target *-*-* } .-6 }

  return 0;
}
