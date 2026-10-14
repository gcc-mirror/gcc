/* With non-call exceptions the first comparison is a separate statement.
   Combining it with the second one must not invert a <= b to a > b.  */
/* { dg-do run } */
/* { dg-options "-O2 -fnon-call-exceptions" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target exceptions } */

__attribute__ ((noipa)) int
f (double a, double b)
{
  if (a < b)
    return 2;
  if (a == b)
    return 2;
  return 0;
}

int
main (void)
{
  if (f (0.0, __builtin_nan ("")) != 0)
    __builtin_abort ();
  return 0;
}
