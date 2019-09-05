/* { dg-do run } */
/* { dg-additional-options "-fgcse-after-reload" } */

int a, b, c, d, e;

static __attribute__ ((__noipa__))
int foo (int i)
{
  __builtin_memmove (&i, &e, 1);
  if (a > 0)
    i /= e;
  e /= 5;
  b = 0;
  return i + c + d + 5;
}

int
main (void)
{
  int x = foo (4);
  if (x != 5)
    __builtin_abort ();
  return 0;
}
