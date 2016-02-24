/* PR middle-end/69909 */
/* { dg-do run { target int128 } } */
/* { dg-additional-options "-w" } */

typedef unsigned V __attribute__ ((vector_size (32)));
typedef __int128 T;
typedef __int128 U __attribute__ ((vector_size (32)));

__attribute__((noinline, noclone)) T
foo (T a, V b, V c, V d, V e, U f)
{
  d[6] ^= 0x10;
  f -= (U) d;
  f[1] |= f[1] << (a & 127);
  c ^= d;
  return b[7] + c[2] + c[2] + d[6] + e[2] + f[1];
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || sizeof (unsigned) != 4 || sizeof (T) != 16)
    return 0;

  T x = foo (1, (V) { 9, 2, 5, 8, 1, 2, 9, 3 },
		(V) { 1, 2, 3, 4, 5, 6, 7, 8 },
		(V) { 4, 1, 2, 9, 8, 3, 5, 2 },
		(V) { 3, 6, 1, 3, 2, 9, 4, 8 }, (U) { 3, 5 });
  if (((unsigned long long) (x >> 64) != 0xffffffffffffffffULL
       || (unsigned long long) x != 0xfffffffe0000001aULL)
      && ((unsigned long long) (x >> 64) != 0xfffffffffffffffdULL
	  || (unsigned long long) x != 0xffffffff00000022ULL))
    __builtin_abort ();
  return 0;
}
