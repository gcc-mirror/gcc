/* { dg-do run } */
/* { dg-additional-options "-O" } */

unsigned g;

unsigned
foo (void)
{
  unsigned a = !g;
  a += !(a % 6);
  return a;
}

int
main ()
{
  unsigned x = foo ();
  if (x != 1)
    __builtin_abort ();
  return 0;
}
