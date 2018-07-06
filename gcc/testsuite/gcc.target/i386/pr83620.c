/* PR rtl-optimization/86620 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -flive-range-shrinkage --param=max-sched-ready-insns=1 -Wno-psabi -mno-avx" } */

typedef unsigned __int128 V __attribute__ ((vector_size (64)));

V u, v;

V
foo (char c, short d, int e, long f, __int128 g)
{
  f >>= c & 63;
  v = (V){f} == u;
  return e + g + v;
}
