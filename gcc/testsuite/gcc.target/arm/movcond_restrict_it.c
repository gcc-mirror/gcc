/* { dg-do run } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-mthumb -O3 -mrestrict-it" }  */

int a;

__attribute__ ((noinline, noclone)) int
fn1 (int c, int d)
{
  if (c > d)
    a = 1;
  else
    a = -1;
  return a;
}

int
main (void)
{
  if (fn1 (4, 5) != -1)
    __builtin_abort ();

  if (fn1 (5, 4) != 1)
    __builtin_abort ();

  return 0;
}
