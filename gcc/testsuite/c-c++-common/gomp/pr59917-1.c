/* PR middle-end/59917 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */

struct J { long buf[8]; };
extern int setjmp (struct J[1]);
extern struct J j[1];
void foo (int);

void
bar (void)
{
  if (setjmp (j) == 0)
    {
      int k;
      foo (-1);
#pragma omp parallel
      for (k = 0; k < 10; ++k)
	foo (k);
      foo (-2);
    }
}
