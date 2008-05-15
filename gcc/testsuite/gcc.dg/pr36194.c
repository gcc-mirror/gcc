/* { dg-do run } */
/* { dg-options "-O2" } */

void abort (void);

__attribute__ ((noinline)) void
f (int i)
{
  if (i != 0x87654321)
    abort ();
  asm ("");
}

__attribute__ ((noinline)) void
g (long long a)
{
  f (a);
  asm ("");
}

main ()
{
  g (0x1234567887654321ll);
  return 0;
}
