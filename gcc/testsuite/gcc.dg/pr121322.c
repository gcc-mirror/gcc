/* PR middle-end/121322 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long long
foo (unsigned long long *p)
{
  unsigned long long a = *p;
  unsigned long long b = __builtin_bswap64 (a);
  return ((b << 32)
	  | ((b >> 8) & 0xff000000ULL)
	  | ((b >> 24) & 0xff0000ULL)
	  | ((b >> 40) & 0xff00ULL));
}
