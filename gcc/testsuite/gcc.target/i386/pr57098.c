/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-msse4 -mcmodel=large" } */

typedef int V __attribute__((vector_size(16)));

void foo (V *p, V *mask)
{
  *p = __builtin_shuffle (*p, *mask);
}
