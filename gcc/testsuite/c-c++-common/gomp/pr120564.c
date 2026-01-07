/* PR middle-end/120564 */
/* { dg-do compile } */

void bar (unsigned long long, unsigned long long, unsigned long long);

void
foo (void)
{
  unsigned long long v1, v2, v3;
#pragma omp parallel for schedule(static, 32) collapse(3)
  for (v1 = 0; v1 < 20; v1 += 2)
    for (v2 = __LONG_LONG_MAX__; v2 > __LONG_LONG_MAX__; v2 -= 3)
      for (v3 = 10; v3 > 0; v3--)
	bar (v1, v2, v3);
}
