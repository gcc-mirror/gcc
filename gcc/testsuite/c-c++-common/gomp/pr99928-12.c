/* PR middle-end/99928 */
/* { dg-do compile } */

int
foo (void)
{
  int l = 0;
  #pragma omp parallel master taskloop simd lastprivate (l) default(none)	/* { dg-bogus "'l' not specified in enclosing 'parallel'" } */
  for (int i = 0; i < 16; i++)
    l = i;
  return l;
}

int
bar (void)
{
  int l = 0;
  #pragma omp parallel master default(none)	/* { dg-message "enclosing 'parallel'" } */
  #pragma omp taskloop simd lastprivate (l)	/* { dg-error "'l' not specified in enclosing 'parallel'" } */
  for (int i = 0; i < 16; i++)
    l = i;
  return l;
}
