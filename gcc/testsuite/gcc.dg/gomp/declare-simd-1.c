/* Test parsing of #pragma omp declare simd */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-old-style-definition" } */

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) \
	    linear (c : 4) simdlen (2) notinbranch
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) \
	    linear (c : 4) simdlen (8) notinbranch
#endif
#pragma omp declare simd uniform (c) aligned (b : 4 * sizeof (int)) linear (a \
									    : 4) simdlen (4) inbranch
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

/* { dg-final { scan-assembler-times "_ZGVnN2uva8l4_f2:" 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVnM2uva8l4_f2:" 1 { target { aarch64*-*-* } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8uva32l4_f2:" 1 { target { i?86-*-* x86_64-*-* } } } } */

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (long long)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (long long)) linear (c : 4) simdlen (8)
#endif
__extension__
long long f3 (long long a, long long *b, long long c);

int
f4 (int x)
{
#ifdef __aarch64__
  #pragma omp declare simd simdlen (2) aligned (b : 4 * sizeof (int))
#else
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
#endif
  __extension__ __extension__ __extension__
  extern int f5 (int a, int *b, int c);
  {
    x++;
#ifdef __aarch64__
    #pragma omp declare simd simdlen (2) linear (c)
#else
    #pragma omp declare simd simdlen (4) linear (c)
#endif
    extern int f6 (int a, int *b, int c);
  }
  return x;
}

#ifdef __aarch64__
#pragma omp declare simd simdlen (4)
#else
#pragma omp declare simd simdlen (16)
#endif
int
f7 (int x)
{
#ifdef __aarch64__
  #pragma omp declare simd simdlen (2) aligned (b : 2 * sizeof (int))
#else
  #pragma omp declare simd simdlen (8) aligned (b : 8 * sizeof (int))
#endif
  extern int f8 (int a, int *b, int c);
  return x;
}

/* { dg-final { scan-assembler-times "_ZGVnM4v_f7:" 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVnN4v_f7:" 1 { target { aarch64*-*-* } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN16v_f7:" 1 { target { i?86-*-* x86_64-*-* } } } } */

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
#endif
int f12 (int c; int *b; int a; int a, int *b, int c);

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
#endif
int
f13 (int c; int *b; int a; int a, int *b, int c)
{
  return a + *b + c;
}

/* { dg-final { scan-assembler-times "_ZGVnM2uva8l4_f13:" 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVnN2uva8l4_f13:" 1 { target { aarch64*-*-* } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8uva32l4_f13:" 1 { target { i?86-*-* x86_64-*-* } } } } */

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
#endif
int
f14 (a, b, c)
     int a, c;
     int *b;
{
  return a + *b + c;
}

/* { dg-final { scan-assembler-times "_ZGVnM2uva8l4_f14:" 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVnN2uva8l4_f14:" 1 { target { aarch64*-*-* } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8uva32l4_f14:" 1 { target { i?86-*-* x86_64-*-* } } } } */

#ifdef __aarch64__
#pragma omp declare simd uniform (a) aligned (b : 2 * sizeof (int)) linear (c : 4) simdlen (2)
#else
#pragma omp declare simd uniform (a) aligned (b : 8 * sizeof (int)) linear (c : 4) simdlen (8)
#endif
int
f15 (int a, int *b, int c)
{
  return a + *b + c;
}

/* { dg-final { scan-assembler-times "_ZGVnM2uva8l4_f15:" 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVnN2uva8l4_f15:" 1 { target { aarch64*-*-* } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN8uva32l4_f15:" 1 { target { i?86-*-* x86_64-*-* } } } } */

#ifdef __aarch64__
#pragma omp declare simd uniform (d) aligned (e : 2 * sizeof (int)) linear (f : 2) simdlen (2)
#else
#pragma omp declare simd uniform (d) aligned (e : 8 * sizeof (int)) linear (f : 4) simdlen (8)
#endif
int f15 (int d, int *e, int f);

#ifdef __aarch64__
#pragma omp declare simd aligned (g : sizeof (*g)) linear (h : 2 * sizeof (g[0]) + sizeof (h)) simdlen (2)
#else
#pragma omp declare simd aligned (g : sizeof (*g)) linear (h : 2 * sizeof (g[0]) + sizeof (h)) simdlen (4)
#endif
int f16 (long *g, int h);

#ifdef __aarch64__
#pragma omp declare simd aligned (h : sizeof (*h)) linear (g : 2 * sizeof (h[0]) + sizeof (g)) simdlen (2)
#else
#pragma omp declare simd aligned (h : sizeof (*h)) linear (g : 2 * sizeof (h[0]) + sizeof (g)) simdlen (4)
#endif
int f17 (int g, long *h)
{
  return g + h[0];
}

/* { dg-final { scan-assembler-times "_ZGVnM2l20va8_f17:" 1 { target { { aarch64*-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVnN2l20va8_f17:" 1 { target { { aarch64*-*-* } && lp64 } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4l20va8_f17:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVbM4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4l12va4_f17:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */

#ifdef __aarch64__
#pragma omp declare simd aligned (i : sizeof (*i)) linear (j : 2 * sizeof (i[0]) + sizeof (j)) simdlen (2)
#else
#pragma omp declare simd aligned (i : sizeof (*i)) linear (j : 2 * sizeof (i[0]) + sizeof (j)) simdlen (4)
#endif
int
f18 (j, i)
     long *i;
     int j;
{
  return j + i[0];
}

/* { dg-final { scan-assembler-times "_ZGVnM2l20va8_f18:" 1 { target { { aarch64*-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVnN2l20va8_f18:" 1 { target { { aarch64*-*-* } && lp64 } } } } */

/* { dg-final { scan-assembler-times "_ZGVbM4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4l20va8_f18:" 1 { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-assembler-times "_ZGVbM4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVbN4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcM4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVcN4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdM4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVdN4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeM4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-assembler-times "_ZGVeN4l12va4_f18:" 1 { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
