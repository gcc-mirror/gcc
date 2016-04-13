/* { dg-do run } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-mthumb -O2 -mrestrict-it" }  */

__attribute__ ((noinline, noclone)) int
fn1 (int a, int b)
{
  return (a == b ? 0 : -1);
}

int
main (void)
{
  if (fn1 (3, 3) != 0)
    __builtin_abort ();

  if (fn1 (6, 7) != -1)
    __builtin_abort ();
}
