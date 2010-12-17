/* PR rtl-optimization/37360 */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O3 -fPIC" } */

typedef unsigned int UQItype __attribute__ ((mode (QI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));

extern const UQItype __popcount_tab[256];
extern int __popcountsi2 (USItype);

int
__popcountsi2 (USItype x)
{
  int i, ret = 0;

  for (i = 0; i < (4 * 8); i += 8)
    ret += __popcount_tab[(x >> i) & 0xff];

  return ret;
}

