/* { dg-do compile } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <stdlib.h>

double    f (double x, double y)	{ return __builtin_fabs  (x) * __builtin_fabs  (y); }
float     g (float x, float y)		{ return __builtin_fabsf (x) * __builtin_fabsf (y); }
int       h (int x, int y)		{ return __builtin_abs   (x) * __builtin_abs   (y); }
long      i (long x, long y)		{ return __builtin_labs  (x) * __builtin_labs  (y); }
long long j (long long x, long long y)	{ return __builtin_llabs (x) * __builtin_llabs (y); }

int       k (int x, int y)		{ return abs   (x) * abs   (y); }
long      l (long x, long y)		{ return labs  (x) * labs  (y); }
long long m (long long x, long long y)	{ return llabs (x) * llabs (y); }

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 8 "optimized" } } */
