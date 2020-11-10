/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=100 -O2" } */

void f (void*);

void g (int *p, int *q)
{
  __SIZE_TYPE__ n = (__SIZE_TYPE__)(p - q);
  if (n < 100)
    f (__builtin_alloca (n)); // { dg-bogus "may be too large" "" { xfail { *-*-* } } }
}
