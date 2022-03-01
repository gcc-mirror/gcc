void add90 (TYPE a[restrict N], TYPE b[restrict N], TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i+=2)
    {
      c[i] = a[i] - b[i+1];
      c[i+1] = a[i+1] + b[i];
    }
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */

void add270 (TYPE a[restrict N], TYPE b[restrict N], TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i+=2)
    {
      c[i] = a[i] + b[i+1];
      c[i+1] = a[i+1] - b[i];
    }
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */

void addMixed (TYPE a[restrict N], TYPE b[restrict N], TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i+=4)
    {
      c[i] = a[i] - b[i+1];
      c[i+1] = a[i+1] + b[i];
      c[i+2] = a[i+2] + b[i+3];
      c[i+3] = a[i+3] - b[i+2];
    }
}

void add90HandUnrolled (TYPE a[restrict N], TYPE b[restrict N],
			TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < (N /2); i+=4)
    {
      c[i] = a[i] - b[i+1];
      c[i+2] = a[i+2] - b[i+3];
      c[i+1] = a[i+1] + b[i];
      c[i+3] = a[i+3] + b[i+2];
    }
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */

void add90Hybrid (TYPE a[restrict N], TYPE b[restrict N], TYPE c[restrict N],
		  TYPE d[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i+=2)
    {
      c[i] = a[i] - b[i+1];
      c[i+1] = a[i+1] + b[i];
      d[i] = a[i] - b[i];
      d[i+1] = a[i+1] - b[i+1];
    }
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 2 "vect" } } */
