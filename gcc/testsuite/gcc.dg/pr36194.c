/* { dg-do run } */
/* { dg-options "-O2" } */

void abort (void);

__attribute__ ((noinline)) void
f (int i)
{
#if(__SIZEOF_INT__ >= 4)
  if (i != 0x87654321)
#else
  if (i != 0x4321)
#endif
    abort ();
  asm ("");
}

__attribute__ ((noinline)) void
g (long long a)
{
  f (a);
  asm ("");
}

int
main ()
{
  g (0x1234567887654321ll);
  return 0;
}
