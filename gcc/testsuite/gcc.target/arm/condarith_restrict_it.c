/* { dg-do run } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-mthumb -O2 -mrestrict-it" }  */

__attribute__ ((noinline, noclone)) void
fn2 ()
{
  __builtin_printf ("4");
}

enum
{
  ONE = 1,
  TWO
} a;

int b;

__attribute__ ((noinline, noclone)) int
fn1 ()
{
  int c = b == 0;
  if (a <= ONE)
    if (b == 0)
      fn2 ();
  if (a)
    if (c)
      a = 0;

  return a;
}

int
main (void)
{
  a = ONE;
  b = 1;
  if (fn1 () != ONE)
    __builtin_abort ();

  a = TWO;
  b = 0;
  if (fn1 () != 0)
    __builtin_abort ();

  return 0;
}
