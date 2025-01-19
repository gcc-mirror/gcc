/* { dg-do run } */

void exit (int);

void __attribute__ ((noinline))
f (unsigned int i)
{
  if ((i & 0xf0000000) != 0xc0000000) __builtin_abort ();
}

int
main ()
{
  f (0xc0000022);
  exit (0);
}
