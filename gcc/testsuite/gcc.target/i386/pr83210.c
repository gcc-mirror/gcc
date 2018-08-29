/* PR target/83210 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\mmul[lq]\M} } } */

void bar (void);

unsigned
f1 (unsigned int x)
{
  unsigned res;
  if (__builtin_mul_overflow (x, 2, &res))
    bar ();
  return res;
}

unsigned long
f2 (unsigned long x)
{
  unsigned long res;
  if (__builtin_mul_overflow (16, x, &res))
    bar ();
  return res;
}

unsigned long long
f3 (unsigned long long x)
{
  unsigned long long res;
  if (__builtin_mul_overflow (x, (1ULL << (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1)), &res))
    bar ();
  return res;
}

#ifdef __SIZEOF_INT128__
unsigned __int128
f4 (unsigned __int128 x)
{
  unsigned __int128 res;
  if (__builtin_mul_overflow (x, (((unsigned __int128) 1) << (__SIZEOF_INT128__ * __CHAR_BIT__ / 2)), &res))
    bar ();
  return res;
}

unsigned __int128
f5 (unsigned __int128 x)
{
  unsigned __int128 res;
  if (__builtin_mul_overflow (x, (((unsigned __int128) 1) << (__SIZEOF_INT128__ * __CHAR_BIT__ / 2 + 3)), &res))
    bar ();
  return res;
}
#endif
