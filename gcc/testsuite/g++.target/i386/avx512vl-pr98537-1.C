/* PR target/98537 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -std=c++11" } */

#ifndef TYPEV
#define TYPEV int
#endif

#ifndef TYPEW
#define TYPEW long long
#endif

#ifndef T_ARR
#define T_ARR					\
  __attribute__ ((target ("avx512vl")))
#endif

typedef TYPEV V __attribute__((__vector_size__(32)));
typedef TYPEW W __attribute__((__vector_size__(32)));

W c, d;
struct B {};
B e;
struct C { W i; };
void foo (C);

C
operator== (B, B)
{
  W r = (V)c == (V)d;
  return {r};
}

void
T_ARR
bar ()
{
  B a;
  foo (a == e);
}
