/* { dg-do compile } */
/* { dg-options "-O2 -mtune=znver4" } */

#ifdef __SIZEOF_INT128__
typedef unsigned long long half;
typedef unsigned __int128 wide;
#else
typedef unsigned int half;
typedef unsigned long long wide;
#endif

#define N (sizeof (half) * 8)

wide
foo (half a, half b)
{
  wide p = (wide) a * b;
  return (p << N) | (p >> N);
}

/* { dg-final { scan-assembler "\[ \t\]xchg\[lq\]" } } */
