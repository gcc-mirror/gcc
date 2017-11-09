/* PR middle-end/60482 */
/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-require-effective-target vect_int } */

double
foo (double *x, int n)
{
  double p = 0.0;
  int i;
  x = __builtin_assume_aligned (x, 128);
  if (n % 128)
    __builtin_unreachable ();
  for (i = 0; i < n; i++)
    p += x[i];
  return p;
}

/* Until fully-masked loops are supported, we always need an epilog
   loop for variable-length vectors.  */
/* { dg-final { scan-tree-dump-not "epilog loop required" "vect" { xfail vect_variable_length } } } */
