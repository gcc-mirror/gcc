/* PR target/110717 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#ifdef __SIZEOF_INT128__
unsigned __int128
foo (unsigned __int128 x)
{
  x <<= 59;
  return ((__int128) x) >> 59;
}
#else
unsigned long long
foo (unsigned long long x)
{
  x <<= 27;
  return ((long long) x) >> 27;
}
#endif

/* { dg-final { scan-assembler-not "sh\[lr\]d" } } */
