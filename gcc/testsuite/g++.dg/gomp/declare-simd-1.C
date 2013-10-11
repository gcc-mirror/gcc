// Test parsing of #pragma omp declare simd
// { dg-do compile }

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

#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
template <typename T>
T f3 (int a, int *b, T c);

template <>
int f3 (int, int *, int);

#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) notinbranch simdlen (4)
template <typename T>
int f4 (int a, int *b, T c)
{
  return a + *b + c;
}

template <>
int f4 (int, int *, int);

template <typename T>
int f5 (int a, int *b, T c);

#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
template <>
int f5 (int a, int *b, int c);

template <int N>
int f6 (int a, int *b, int c);

#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) inbranch simdlen (4)
template <>
int f6<3> (int a, int *b, int c);

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (long long)) linear (c : 4) simdlen (8)
__extension__
long long f7 (long long a, long long *b, long long c);

#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) notinbranch simdlen (8)
extern "C"
int f8 (int a, int *b, int c);

extern "C"
{
  #pragma omp declare simd
  int f9 (int a, int *b, int c);
}

namespace N1
{
  namespace N2
  {
    #pragma omp declare simd simdlen (2) aligned (b : sizeof (long long) * 2)
    __extension__ long long
    f10 (long long *b)
    {
      return *b;
    }
  }
}

struct A
{
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
  int f11 (int a, int *b, int c);

  #pragma omp declare simd
  template <int N>
  int f12 (int a, int *b, int c);

  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) notinbranch simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4) inbranch
  static int f13 (int a, int *b, int c);

  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
  int f14 (int a, int *b, int c) { return a + *b + c; }

  #pragma omp declare simd
  template <int N>
  int f15 (int a, int *b, int c) { return a + *b + c; }

  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
  static int f16 (int a, int *b, int c) { return a + *b + c; }
};

template <>
int A::f12<2> (int, int *, int);

template <>
int A::f15<2> (int, int *, int);

template <typename T>
struct B
{
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8) notinbranch
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4) inbranch
  int f17 (int a, int *b, int c);

  #pragma omp declare simd
  template <int N>
  int f18 (int a, int *b, int c);

  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
  static int f19 (int a, int *b, int c);

  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
  int f20 (int a, int *b, int c) { return a + *b + c; }

  #pragma omp declare simd
  template <int N>
  int f21 (int a, int *b, int c) { return a + *b + c; }

  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
  static int f22 (int a, int *b, int c) { return a + *b + c; }

  template <int N>
  int f23 (int, int *, int);

  template <int N>
  static int f24 (int, int *, int);

  template <int N>
  int f25 (int, int *, int);

  template <int N>
  static int f26 (int, int *, int);
};

B <int> b;

template <>
template <>
int B<int>::f18<0> (int, int *, int);

template <>
template <>
int B<int>::f21<9> (int, int *, int);

#pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int)) uniform (a, c)
template <>
template <>
int B<int>::f23<7> (int a, int *b, int c);

#pragma omp declare simd simdlen (4) aligned (b : 8 * sizeof (int)) linear (a, c : 2)
template <>
template <>
int B<int>::f24<-1> (int a, int *b, int c);

#pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int)) uniform (a, c)
template <>
template <>
int B<int>::f25<7> (int a, int *b, int c)
{
  return a + *b + c;
}

#pragma omp declare simd simdlen (4) aligned (b : 8 * sizeof (int)) linear (a, c : 2)
template <>
template <>
int B<int>::f26<-1> (int a, int *b, int c)
{
  return a + *b + c;
}

int
f27 (int x)
{
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
  extern int f28 (int a, int *b, int c);
  {
    x++;
    #pragma omp declare simd simdlen (4) linear (c)
    extern int f29 (int a, int *b, int c);
  }
  return x;
}

#pragma omp declare simd simdlen (16)
int
f30 (int x)
{
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
  extern int f31 (int a, int *b, int c);
  return x;
}

template <int N>
struct C
{
  #pragma omp declare simd simdlen (N) aligned (a : N * sizeof (int)) linear (c : N) notinbranch
  int f32 (int a, int *b, int c);
};

C <2> c;

int
f33 (int x)
{
  if (x)
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f34 (int a, int *b, int c);
  while (x < 10)
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
    extern int f35 (int a, int *b, int c);
  return x;
}

#pragma omp declare simd simdlen (N)
template <int N>
int f36 (int);

struct D
{
  int d;
  #pragma omp declare simd simdlen (N) linear (a : sizeof (a) + sizeof (d) + sizeof (this) + sizeof (this->d))
  template <int N>
  int f37 (int a);
  int e;
};

void
f38 (D &d)
{
  d.f37 <12> (6);
}
