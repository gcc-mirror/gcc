/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds -fdump-tree-cunroll-details" } */

typedef struct { unsigned data; } s1;
s1 g_x[4];

extern void foo (s1 *x1, s1 *x2, int a, int b)
{
  int i;
  for(i = 0; i < a; i++)
    if(i == b)
      g_x[i] = *x1;
    else
      g_x[i] = *x2;
}

/* { dg-final { scan-tree-dump "Loop 1 iterates at most 3 times" "cunroll" } } */
