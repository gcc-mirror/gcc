/* PR middle-end/59152 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fipa-pure-const" } */

extern int b[];
void
foo (void)
{
  unsigned long v1, v2, v3;
  #pragma omp parallel for schedule(static, 32) collapse(3)
    for (v1 = 0; v1 < 20; v1 += 2)
      for (v2 = __LONG_MAX__; v2 > __LONG_MAX__ - 30; v2 -= 3)
	for (v3 = 10; v3 > 0; v3--)
	  #pragma omp atomic
	    b[v3]++;
}

void
bar (void)
{
  unsigned long v1, v2, v3;
  #pragma omp parallel for schedule(static) collapse(3)
    for (v1 = 0; v1 < 20; v1 += 2)
      for (v2 = __LONG_MAX__; v2 > __LONG_MAX__ - 30; v2 -= 3)
	for (v3 = 10; v3 > 0; v3--)
	  #pragma omp atomic
	    b[v3]++;
}

void
baz (void)
{
  unsigned long v1, v2, v3;
  #pragma omp parallel for schedule(runtime) collapse(3)
    for (v1 = 0; v1 < 20; v1 += 2)
      for (v2 = __LONG_MAX__; v2 > __LONG_MAX__ - 30; v2 -= 3)
	for (v3 = 10; v3 > 0; v3--)
	  #pragma omp atomic
	    b[v3]++;
}
