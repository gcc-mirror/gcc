/* PR target/55147 */
/* { dg-do run } */
/* { dg-options "-O1" } */
/* { dg-additional-options "-march=i486" { target ia32 } } */

extern void abort (void);

__attribute__((noclone, noinline)) unsigned int
foo (unsigned long long *p, int i)
{
  return __builtin_bswap64 (p[i]);
}

int
main ()
{
  unsigned long long p[64];
  int i;
  for (i = 0; i < 64; i++)
    p[i] = 0x123456789abcdef0ULL ^ (1ULL << i) ^ (1ULL << (63 - i));
  for (i = 0; i < 64; i++)
    if (foo (p, i) != __builtin_bswap32 (p[i] >> 32))
      abort ();
  return 0;
}
