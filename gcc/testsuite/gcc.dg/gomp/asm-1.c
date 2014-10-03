/* PR middle-end/30263 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */

extern int omp_get_thread_num (void);

void
foo (void)
{
  int s0, s1 = 5, s2 = 6;
  int p0, p1, p2;
  int f0 = 4, f1 = 5, f2 = 6;
#pragma omp parallel shared (s0, s1, s2) private (p0, p1, p2) \
	    firstprivate (f0, f1, f2)
  {
    asm ("" : "=m" (p0) : "m" (p1), "mr" (p2));
    if (omp_get_thread_num () == 0)
      asm ("" : "=m" (s0) : "m" (s1), "mr" (s2));
    asm ("" : "=m" (f0) : "m" (f1), "mr" (f2));
  }
}
