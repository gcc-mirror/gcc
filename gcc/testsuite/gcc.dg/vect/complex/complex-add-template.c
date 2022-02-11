#include <complex.h>

void add0 (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	   _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = a[i] + b[i];
}

void add90snd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	       _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = a[i] + (b[i] * I);
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */

void add180snd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = a[i] + (b[i] * I * I);
}

void add270snd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = a[i] + (b[i] * I * I * I);
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */

void add90fst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	       _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = (a[i] * I) + b[i];
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */

void add180fst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = (a[i] * I * I) + b[i];
}

void add270fst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	        _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = (a[i] * I * I * I) + b[i];
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */

void addconjfst (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		 _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = ~a[i] + b[i];
}

void addconjsnd (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		 _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = a[i] + ~b[i];
}

void addconjboth (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		  _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = ~a[i] + ~b[i];
}
