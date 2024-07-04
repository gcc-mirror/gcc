// Test parsing of #pragma omp declare simd
// { dg-do compile }
// { dg-options "-fopenmp -ffat-lto-objects" }

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) \
	    linear (c : 4) simdlen (2) notinbranch
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) \
	    linear (c : 4) simdlen (8) notinbranch
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a \
									    : 4) simdlen (4) inbranch
#endif
int f1 (int a, int *b, int c);

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
#endif
int f2 (int a, int *b, int c)
{
  return a + *b + c;
}

// { dg-final { scan-assembler-times "_ZGVnN2uva8l4__Z2f2iPii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2uva8l4__Z2f2iPii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8uva32l4__Z2f2iPii:" 1 { target { i?86-*-* x86_64-*-* } } } }

#ifdef __aarch64__
#pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
template <typename T>
T f3 (int a, int *b, T c);

template <>
int f3 (int, int *, int);

#ifdef __aarch64__
#pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) notinbranch simdlen (2)
#else
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) notinbranch simdlen (4)
#endif
template <typename T>
int f4 (int a, int *b, T c)
{
  return a + *b + c;
}

template <>
int f4 (int, int *, int);

template <typename T>
int f5 (int a, int *b, T c);

#ifdef __aarch64__
#pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
template <>
int f5 (int a, int *b, int c);

template <int N>
int f6 (int a, int *b, int c);

#ifdef __aarch64__
#pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) inbranch simdlen (2)
#else
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) inbranch simdlen (4)
#endif
template <>
int f6<3> (int a, int *b, int c);

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (long long)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (long long)) linear (c : 4) simdlen (8)
#endif
__extension__
long long f7 (long long a, long long *b, long long c);

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) notinbranch simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) notinbranch simdlen (8)
#endif
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

// { dg-final { scan-assembler-times "_ZGVnN2va16__ZN2N12N23f10EPx:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2va16__ZN2N12N23f10EPx:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN2va16__ZN2N12N23f10EPx:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2va16__ZN2N12N23f10EPx:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnN2va16__ZN2N12N23f10EPx:" 1 { target { aarch64*-*-* } } } }

struct A
{
#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
  int f11 (int a, int *b, int c);

  #pragma omp declare simd
  template <int N>
  int f12 (int a, int *b, int c);

#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) notinbranch simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2) inbranch
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) notinbranch simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4) inbranch
#endif
  static int f13 (int a, int *b, int c);

#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
  int f14 (int a, int *b, int c) { return a + *b + c; }

  #pragma omp declare simd
  template <int N>
  int f15 (int a, int *b, int c) { return a + *b + c; }

#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
  static int f16 (int a, int *b, int c) { return a + *b + c; }
};

template <>
int A::f12<2> (int, int *, int);

template <>
int A::f15<2> (int, int *, int);

template <typename T>
struct B
{
#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2) notinbranch
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2) inbranch
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8) notinbranch
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4) inbranch
#endif
  int f17 (int a, int *b, int c);

  #pragma omp declare simd
  template <int N>
  int f18 (int a, int *b, int c);

#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
  static int f19 (int a, int *b, int c);

#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
  int f20 (int a, int *b, int c) { return a + *b + c; }

  #pragma omp declare simd
  template <int N>
  int f21 (int a, int *b, int c) { return a + *b + c; }

#ifdef __aarch64__
  #pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
  #pragma omp declare simd uniform (c) aligned (b : 2 * sizeof (int)) linear (a : 4) simdlen (2)
#else
  #pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
  #pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a : 4) simdlen (4)
#endif
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

#ifdef __aarch64__
#pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int)) uniform (a, c)
#else
#pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int)) uniform (a, c)
#endif
template <>
template <>
int B<int>::f23<7> (int a, int *b, int c);

#ifdef __aarch64__
#pragma omp declare simd simdlen (2) aligned (b : 4 * sizeof (int)) linear (a, c : 2)
#else
#pragma omp declare simd simdlen (4) aligned (b : 8 * sizeof (int)) linear (a, c : 2)
#endif
template <>
template <>
int B<int>::f24<-1> (int a, int *b, int c);

#ifdef __aarch64__
#pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int)) uniform (a, c)
#else
#pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int)) uniform (a, c)
#endif
template <>
template <>
int B<int>::f25<7> (int a, int *b, int c)
{
  return a + *b + c;
}
// { dg-final { scan-assembler-times "_ZGVnN2vuva8u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2vuva8u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN8vuva32u__ZN1BIiE3f25ILi7EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }

#ifdef __aarch64__
#pragma omp declare simd simdlen (2) aligned (b : 4 * sizeof (int)) linear (a, c : 2)
#else
#pragma omp declare simd simdlen (4) aligned (b : 8 * sizeof (int)) linear (a, c : 2)
#endif
template <>
template <>
int B<int>::f26<-1> (int a, int *b, int c)
{
  return a + *b + c;
}

// { dg-final { scan-assembler-times "_ZGVnN2vl2va16__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM2vl2va16__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN4vl2va32__ZN1BIiE3f26ILin1EEEiiPii:" 1 { target { i?86-*-* x86_64-*-* } } } }

int
f27 (int x)
{
#ifdef __aarch64__
  #pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int))
#else
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
#endif
  extern int f28 (int a, int *b, int c);
  {
    x++;
#ifdef __aarch64__
    #pragma omp declare simd simdlen (2) linear (c)
#else
    #pragma omp declare simd simdlen (4) linear (c)
#endif
    extern int f29 (int a, int *b, int c);
  }
  return x;
}

#ifdef __aarch64__
#pragma omp declare simd simdlen (4)
#else
#pragma omp declare simd simdlen (16)
#endif
int
f30 (int x)
{
#ifdef __aarch64__
  #pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int))
#else
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
#endif

  extern int f31 (int a, int *b, int c);
  return x;
}

// { dg-final { scan-assembler-times "_ZGVnN4v__Z3f30i:" 1 { target { aarch64*-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVnM4v__Z3f30i:" 1 { target { aarch64*-*-* } } } }

// { dg-final { scan-assembler-times "_ZGVbM16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16v__Z3f30i:" 1 { target { i?86-*-* x86_64-*-* } } } }

template <int N>
struct C
{
  #pragma omp declare simd simdlen (N) aligned (b : N * sizeof (int)) linear (c : N) notinbranch
  int f32 (int a, int *b, int c);
};

C <2> c;

int
f33 (int x)
{
  if (x)
#ifdef __aarch64__
    #pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int))
#else
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
#endif
    extern int f34 (int a, int *b, int c);
  while (x < 10)
#ifdef __aarch64__
    #pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int))
#else
    #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
#endif
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
#ifdef __aarch64__
  d.f37 <2> (6);
#else
  d.f37 <16> (6);
#endif
}
