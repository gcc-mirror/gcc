/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fvect-cost-model=unlimited -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 6 "vect" } } */

typedef struct { _Complex double c; double a1; double a2;}
  cdf;
typedef struct { _Complex double c; double a1; double a2; double a3; double a4;}
  cdf2;
typedef struct { _Complex double c1; _Complex double c2; double a1; double a2; double a3; double a4;}
  cdf3;
typedef struct { _Complex double c1; _Complex double c2; double a1; double a2;}
  cdf4;

#define N 100
/* VMAT_ELEMENTWISE.  */
void
__attribute__((noipa))
foo (cdf* a, cdf* __restrict b)
{
   for (int i = 0; i < N; ++i)
    {
      a[i].c = b[i].c;
      a[i].a1 = b[i].a1;
      a[i].a2 = b[i].a2;
    }
}

/* VMAT_CONTIGUOUS_PERMUTE.  */
void
__attribute__((noipa))
foo1 (cdf2* a, cdf2* __restrict b)
{
   for (int i = 0; i < N; ++i)
    {
      a[i].c = b[i].c;
      a[i].a1 = b[i].a1;
      a[i].a2 = b[i].a2;
      a[i].a3 = b[i].a3;
      a[i].a4 = b[i].a4;
    }
}

/* VMAT_CONTIGUOUS.  */
void
__attribute__((noipa))
foo2 (cdf3* a, cdf3* __restrict b)
{
   for (int i = 0; i < N; ++i)
    {
      a[i].c1 = b[i].c1;
      a[i].c2 = b[i].c2;
      a[i].a1 = b[i].a1;
      a[i].a2 = b[i].a2;
      a[i].a3 = b[i].a3;
      a[i].a4 = b[i].a4;
    }
}

/* VMAT_STRIDED_SLP.  */
void
__attribute__((noipa))
foo3 (cdf4* a, cdf4* __restrict b)
{
   for (int i = 0; i < N; ++i)
    {
      a[i].c1 = b[i].c1;
      a[i].c2 = b[i].c2;
      a[i].a1 = b[i].a1;
      a[i].a2 = b[i].a2;
    }
}

/* VMAT_CONTIGUOUS_REVERSE.  */
void
__attribute__((noipa))
foo4 (_Complex double* a, _Complex double* __restrict b)
{
  for (int i = 0; i != N; i++)
    a[i] = b[N-i-1];
}

/* VMAT_CONTIGUOUS_DOWN.  */
void
__attribute__((noipa))
foo5 (_Complex double* a, _Complex double* __restrict b)
{
  for (int i = 0; i != N; i++)
    a[N-i-1] = b[0];
}
