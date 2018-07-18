// PR middle-end/78025
// { dg-do compile }
// { dg-additional-options "-O2" }

struct S { S (); ~S (); };

int bar1 (int, int, float &, S &, int *, int, int &, int &, int &, int &, int &);
int bar2 (int, int, float &, S &, int *, int, int &, int &, int &, int &, int &);
int bar3 (int, int, float &, S &, int *, int, int &, int &, int &, int &, int &) __attribute__((noreturn));
int bar4 (int, int, float &, S &, int *, int, int &, int &, int &, int &, int &) __attribute__((noreturn));

#pragma omp declare simd notinbranch uniform (b, c, d, e) aligned (e : 16) \
		    linear (f : 2) linear (ref (g) : 1) \
		    linear (val (h) : 1) linear (uval (i) : 1) \
		    linear (k : 4)
int
foo1 (int a, int b, float c, S d, int *e, int f, int &g, int &h, int &i, int j, int k)
{
  return bar1 (a, b, c, d, e, f, g, h, i, j, k);
}

#pragma omp declare simd inbranch uniform (b, c, d, e) aligned (e : 16) \
		    linear (f : 2) linear (ref (g) : 1) \
		    linear (val (h) : 1) linear (uval (i) : 1) \
		    linear (k : 4)
int
foo2 (int a, int b, float c, S d, int *e, int f, int &g, int &h, int &i, int j, int k)
{
  return bar2 (a, b, c, d, e, f, g, h, i, j, k);
}

#pragma omp declare simd notinbranch uniform (b, c, d, e) aligned (e : 16) \
		    linear (f : 2) linear (ref (g) : 1) \
		    linear (val (h) : 1) linear (uval (i) : 1) \
		    linear (k : 4)
int
foo3 (int a, int b, float c, S d, int *e, int f, int &g, int &h, int &i, int j, int k)
{
  return bar3 (a, b, c, d, e, f, g, h, i, j, k);
}

#pragma omp declare simd inbranch uniform (b, c, d, e) aligned (e : 16) \
		    linear (f : 2) linear (ref (g) : 1) \
		    linear (val (h) : 1) linear (uval (i) : 1) \
		    linear (k : 4)
int
foo4 (int a, int b, float c, S d, int *e, int f, int &g, int &h, int &i, int j, int k)
{
  return bar4 (a, b, c, d, e, f, g, h, i, j, k);
}
