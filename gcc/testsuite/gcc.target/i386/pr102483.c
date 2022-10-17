/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -ftree-vectorize -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MIN" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MAX" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_PLUS" 1 "optimized" } } */

char
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_add (char* p)
{
  char sum = 0;
  for (int i = 0; i != 4; i++)
    sum += p[i];
  return sum;
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))

unsigned char
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_umax (unsigned char* p)
{
  unsigned char sum = p[0];
  for (int i = 0; i != 4; i++)
    sum = MAX(sum, p[i]);
  return sum;
}

unsigned char
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_umin (unsigned char* p)
{
  unsigned char sum = p[0];
  for (int i = 0; i != 4; i++)
    sum = MIN(sum, p[i]);
  return sum;
}

char
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_smax (char* p)
{
  char sum = p[0];
  for (int i = 0; i != 4; i++)
    sum = MAX(sum, p[i]);
  return sum;
}

char
__attribute__((noipa, optimize("Ofast"),target("sse4.1")))
reduce_smin (char* p)
{
  char sum = p[0];
  for (int i = 0; i != 4; i++)
    sum = MIN(sum, p[i]);
  return sum;
}
