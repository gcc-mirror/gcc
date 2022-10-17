/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\.REDUC_PLUS" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MIN" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MAX" 2 "optimized" } } */

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))

short
__attribute__((noipa, optimize("Ofast"),target("sse2")))
reduce_add (short* __restrict pa)
{
  short sum = 0;
  for (int i = 0; i != 4; i++)
    sum += pa[i];
  return sum;
}

short
__attribute__((noipa, optimize("Ofast"),target("sse2")))
reduce_smax (short* __restrict pa)
{
  short sum = pa[0];
  for (int i = 0; i != 4; i++)
    sum = MAX(sum, pa[i]);
  return sum;
}

short
__attribute__((noipa, optimize("Ofast"),target("sse2")))
reduce_smin (short* __restrict pa)
{
  short sum = pa[0];
  for (int i = 0; i != 4; i++)
    sum = MIN(sum, pa[i]);
  return sum;
}

unsigned short
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_umax (unsigned short* __restrict pa)
{
  unsigned short sum = pa[0];
  for (int i = 0; i != 4; i++)
    sum = MAX(sum, pa[i]);
  return sum;
}

unsigned short
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_umin (unsigned short* __restrict pa)
{
  unsigned short sum = pa[0];
  for (int i = 0; i != 4; i++)
    sum = MIN(sum, pa[i]);
  return sum;
}
