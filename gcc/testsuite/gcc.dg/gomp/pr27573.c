/* PR middle-end/27573 */
/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O2 -fopenmp -fprofile-generate" } */

extern int puts (const char *);

int
main (void)
{
  int i, j = 8;
#pragma omp parallel
  {
    puts ("foo");
    for (i = 1; i < j - 1; i++)
      ;
  }
  return 0;
}
