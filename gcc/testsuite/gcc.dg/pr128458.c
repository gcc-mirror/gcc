/* { dg-do run } */
/* { dg-options "-Ofast" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64_runtime } */

/* There is no __builtin_powi variant for _Float64 and reassoc dropped
   the pow call from the product after linearizing it.  */

extern double pow (double, double);

__attribute__((noipa)) _Float64
f (_Float64 x)
{
  return x * x * pow (x, 3.0);
}

int
main (void)
{
  if (f (2) != 32)
    __builtin_abort ();
  return 0;
}
