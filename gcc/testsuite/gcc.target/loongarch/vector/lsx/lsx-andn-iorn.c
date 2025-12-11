/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -ftree-vectorize -fdump-tree-optimized" } */

#ifndef N
#define N 4
#endif

extern float a[N], b[N];
extern int c[N], d[N];

void
bar1 (void)
{
  for (int i = 0; i < N; i++)
    d[i] = a[i] > b[i] ? 0 : c[i];
}

void
bar2 (void)
{
  for (int i = 0; i < N; i++)
    d[i] = a[i] > b[i] ? c[i]: -1;
}

/* We should produce a BIT_ANDC and BIT_IORC here.  */

/* { dg-final { scan-tree-dump ".BIT_ANDN " "optimized" } } */
/* { dg-final { scan-tree-dump ".BIT_IORN " "optimized" } } */
