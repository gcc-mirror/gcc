/* Test parsing of #pragma omp declare simd */
/* { dg-do compile } */

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) \
	    linear (c : 4) simdlen (8) notinbranch
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a \
									    : 4) simdlen (4) inbranch
int f1 (int a, int *b, int c);

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
int f2 (int a, int *b, int c)
{
  return a + *b + c;
}

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (long long)) linear (c : 4) simdlen (8)
__extension__
long long f3 (long long a, long long *b, long long c);

int
f4 (int x)
{
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
  __extension__ __extension__ __extension__
  extern int f5 (int a, int *b, int c);
  {
    x++;
    #pragma omp declare simd simdlen (4) linear (c)
    extern int f6 (int a, int *b, int c);
  }
  return x;
}

#pragma omp declare simd simdlen (16)
int
f7 (int x)
{
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
  extern int f8 (int a, int *b, int c);
  return x;
}

int
f9 (int x)
{
  if (x)
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f10 (int a, int *b, int c);
  while (x < 10)
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f11 (int a, int *b, int c);
  return x;
}

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
int f12 (int c; int *b; int a; int a, int *b, int c);

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
int
f13 (int c; int *b; int a; int a, int *b, int c)
{
  return a + *b + c;
}

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
int
f14 (a, b, c)
     int a, c;
     int *b;
{
  return a + *b + c;
}

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
int
f15 (int a, int *b, int c)
{
  return a + *b + c;
}

#pragma omp declare simd uniform (d) aligned (e : 8 * sizeof (int)) linear (f : 4) simdlen (8)
int f15 (int d, int *e, int f);

#pragma omp declare simd aligned (g : sizeof (*g)) linear (h : 2 * sizeof (g[0]) + sizeof (h)) simdlen (4)
int f16 (long *g, int h);

#pragma omp declare simd aligned (h : sizeof (*h)) linear (g : 2 * sizeof (h[0]) + sizeof (g)) simdlen (4)
int f17 (int g, long *h)
{
  return g + h[0];
}

#pragma omp declare simd aligned (i : sizeof (*i)) linear (j : 2 * sizeof (i[0]) + sizeof (j)) simdlen (4)
int
f18 (j, i)
     long *i;
     int j;
{
  return j + i[0];
}
