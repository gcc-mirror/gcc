/* { dg-do run { target le } } */

short c;

__attribute__((__noipa__)) short
foo (short s)
{
  __builtin_memmove (1 + (char *) &s, &c, 1);
  return s >> 10;
}

int
main ()
{
  short x = foo (-6);
  if (x)
    __builtin_abort();
}
