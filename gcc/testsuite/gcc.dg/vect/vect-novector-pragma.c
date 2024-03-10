/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void f1 (int * restrict a, int * restrict b, int n)
{
#pragma GCC novector
    for (int i = 0; i < (n & -8); i++)
      a[i] += b[i];
}

void f2 (int * restrict a, int * restrict b, int n)
{
#pragma GCC novector
#pragma GCC ivdep
#pragma GCC unroll 2
    for (int i = 0; i < (n & -8); i++)
      a[i] += b[i];
}

void f3 (int * restrict a, int * restrict b, int n)
{
#pragma GCC ivdep
#pragma GCC novector
#pragma GCC unroll 2
    for (int i = 0; i < (n & -8); i++)
      a[i] += b[i];
}

void f4 (int * restrict a, int * restrict b, int n)
{
#pragma GCC ivdep
#pragma GCC unroll 2
#pragma GCC novector
    for (int i = 0; i < (n & -8); i++)
      a[i] += b[i];
}

void f5 (int * restrict a, int * restrict b, int n)
{
    int i = 0;
#pragma GCC novector
    do
      {
        a[i] += b[i];
        i++;
      }
    while (i < (n & -8));
}

void f6 (int * restrict a, int * restrict b, int n)
{
    int i = 0;
#pragma GCC novector
    while (i < (n & -8))
      {
        a[i] += b[i];
        i++;
      }
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
