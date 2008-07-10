/* PR middle-end/36790 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (char b)
{
}

void
bar (char b)
{
  foo (b);
  #pragma omp task default (shared)
    b = 0;
}

int
main ()
{
  bar (0);
  return 0;
}
