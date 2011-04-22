/* PR middle-end/48591 */
/* { dg-do compile { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "-fopenmp" } */

extern void abort (void);

int
main ()
{
  __float128 f = 0.0;
  int i;
  #pragma omp parallel for reduction(+:f)
    for (i = 0; i < 128; i++)
      f += 0.5Q;
  if (f != 64.0Q)
    abort ();
  #pragma omp atomic
    f += 8.5Q;
  if (f != 72.5Q)
    abort ();
  return 0;
}
