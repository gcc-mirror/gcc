/* { dg-do run } */
/* { dg-options "-O -favoid-store-forwarding" } */

int c;

short
foo (short s)
{
  __builtin_memset (&s, c, 1);
  return s;
}

int
main ()
{
  short x = foo (0x1111);
  if (x != 0x1100 && x != 0x0011)
    __builtin_abort();
}
