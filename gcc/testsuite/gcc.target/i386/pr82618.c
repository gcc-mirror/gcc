/* PR target/82618 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 U;
typedef unsigned long long H;
#else
typedef unsigned long long U;
typedef unsigned int H;
#endif

H f0 (U x, U y)
{
  return (x - y) >> (__CHAR_BIT__ * sizeof (H));
}

/* { dg-final { scan-assembler {\mcmp} } } */
