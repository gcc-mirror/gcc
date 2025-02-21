/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=100 -O2" } */

void f (void*);

void g (int *p, int *q)
{
  __SIZE_TYPE__ n = (__SIZE_TYPE__)(p - q);
  if (n < 100)
    f (__builtin_alloca (n));
}
