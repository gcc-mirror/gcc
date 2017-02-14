/* PR middle-end/72781 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -Wuninitialized" } */

int u;

void
foo (int *p)
{
  int i;
  #pragma omp for simd lastprivate(u) schedule (static, 32)	/* { dg-bogus "may be used uninitialized in this function" } */
  for (i = 0; i < 1024; i++)
    u = p[i];
}

void
bar (int *p)
{
  int i;
  #pragma omp taskloop simd lastprivate(u)	/* { dg-bogus "may be used uninitialized in this function" } */
  for (i = 0; i < 1024; i++)
    u = p[i];
}
