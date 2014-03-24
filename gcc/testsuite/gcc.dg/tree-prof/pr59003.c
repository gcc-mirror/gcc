/* PR target/59003 */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=amdfam10" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fno-common" { target hppa*-*-hpux* } } */

__attribute__((noinline, noclone)) void *
foo (void *p, unsigned int q)
{
  return __builtin_memset (p, 0, q * 4UL);
}

char buf[128] __attribute__((aligned (32)));

int
main ()
{
  int i;
  for (i = 0; i < 100000; i++)
    foo (buf + 4, 1 + (i & 1));
  for (i = 0; i < 128; i++)
    {
      buf[i] = 'X';
      asm volatile ("" : : : "memory");
    }
  foo (buf + 32, 7);
  for (i = 0; i < 128; i++)
    if (buf[i] != ((i < 32 || i >= 32 + 28) ? 'X' : 0))
      __builtin_abort ();
  return 0;
}
