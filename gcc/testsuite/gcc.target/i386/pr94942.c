/* PR target/94942 */
/* { dg-do compile } */
/* { dg-options "-O -flive-range-shrinkage -ftree-vrp -mavx512vl -mno-avx512bw -Wno-div-by-zero" } */

typedef unsigned __attribute__((__vector_size__(8))) U;
typedef short __attribute__((__vector_size__(8))) V;
typedef char __attribute__((__vector_size__(16))) W;
typedef int __attribute__((__vector_size__(16))) Z;
int i, j, n, o;
W k;
Z l;
char m;

U
foo (U q, long long r, V s)
{
  Z t = (i & i - (Z){10} & 4) - (0 != j);
  Z u = o * (j * l);
  s -= (char)__builtin_clrsbll (n);
  W v = (k | k >> m + (W){4}) % 0;
  W w = v + (W)t + (W)u;
  U x = ((union { W a; U b; })w).b + q + (U)s + (U)r;
  return x;
}
