// PR tree-optimization/83523
// { dg-do compile }
// { dg-options "-O2 -fexceptions -fnon-call-exceptions -ftrapv" }

#ifdef __SIZEOF_INT128__
typedef __int128 T;
typedef long long int U;
#else
typedef long long int T;
typedef int U;
#endif

struct S { S (); ~S (); };
void bar ();

T
foo (U x, U y)
{
  T z = x;
  S s;
  bar ();
  z *= y;
  bar ();
  return z;
}
