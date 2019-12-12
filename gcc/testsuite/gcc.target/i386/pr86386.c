/* PR target/86386 */
/* { dg-do run { target { avx_runtime && int128 } } } */
/* { dg-options "-Os -fno-tree-dce -mstringop-strategy=vector_loop -mavx" } */

unsigned c, d, e, f;

unsigned __attribute__((noipa))
foo (unsigned char g, unsigned short h, unsigned i, unsigned long long j,
     unsigned char k, unsigned short l, unsigned m, unsigned __int128 n)
{
  __builtin_memset (&e, 0, 3);
  n <<= m;
  __builtin_memcpy (&m, 2 + (char *) &n, 1);
  m >>= 0;
  d ^= __builtin_mul_overflow (l, n, &m);
  return m;
}

int
main ()
{
  unsigned __int128 x = foo (0, 0, 0, 0, 0, 4, 1, 3);
  if (x != 24)
    __builtin_abort ();
  return 0;
}
