/* { dg-do run } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-mthumb -O -mrestrict-it" }  */

int a;

__attribute__((noinline, noclone)) int
fn1 (int c, int d)
{
  a -= c == d;
  return a;
}

int
main (void)
{
  a = 10;
  if (fn1 (4, 4) != 9)
    __builtin_abort ();

  a = 5;
  if (fn1 (3, 4) != 5)
    __builtin_abort ();

  return 0;
}
