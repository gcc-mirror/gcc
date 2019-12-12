/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-mavx2" } */

typedef float __v4sf __attribute__((vector_size (16)));
typedef int __v4si __attribute__((vector_size (16)));
typedef float __v8sf __attribute__((vector_size (32)));
typedef int __v8si __attribute__((vector_size (32)));
__v4si f1 (__v4sf, __v4sf, float *);
__v8si f2 (__v8sf, __v8sf, float *);
__v4si f3 (__v4si, int, __v4si);

#pragma omp declare variant (f1) match (construct={parallel,for,simd(simdlen(4),notinbranch,uniform(z),aligned(z:4 * sizeof (*z)))})
int f4 (float x, float y, float *z);
#pragma omp declare variant (f1) match (construct={parallel,for,simd(uniform(w),simdlen(8*2-12),aligned(w:4*sizeof (float)),notinbranch)})
int f5 (float u, float v, float *w);
#pragma omp declare variant (f1) match (construct={parallel,for,simd(linear(w),notinbranch,simdlen(4),aligned(w:4*sizeof (float)))})	/* { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" "" { target c } } */
int f6 (float u, float v, float *w);
#pragma omp declare variant (f1) match (construct={parallel,for,simd(uniform(w),notinbranch,simdlen(4),aligned(w:2*sizeof (float)))})	/* { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" "" { target c } } */
int f7 (float u, float v, float *w);
#pragma omp declare variant (f1) match (construct={parallel,for,simd(uniform(w),notinbranch,simdlen(4),aligned(w))})			/* { dg-error "'f1' used as a variant with incompatible 'construct' selector sets" "" { target c } } */
int f8 (float u, float v, float *w);
#pragma omp declare variant (f2) match (construct={for,simd(uniform(z),simdlen(8),notinbranch)})
int f9 (float x, float y, float *z);
#pragma omp declare variant (f2) match (construct={for,simd(notinbranch,simdlen(2+2+4),uniform (q))})
int f10 (float x, float y, float *q);
#pragma omp declare variant (f2) match (construct={for,simd(linear(z:2),simdlen(8),notinbranch)})	/* { dg-error "'f2' used as a variant with incompatible 'construct' selector sets" "" { target c } } */
int f11 (float x, float y, float *z);
#pragma omp declare variant (f3) match (construct={simd(simdlen(4),inbranch,linear(y:1))})
int f12 (int x, int y);
#pragma omp declare variant (f3) match (construct={simd(inbranch, simdlen (5-1), linear (q:4-3))})
int f13 (int x, int q);
#pragma omp declare variant (f3) match (construct={simd(inbranch,simdlen(4),linear(q:2))})		/* { dg-error "'f3' used as a variant with incompatible 'construct' selector sets" "" { target c } } */
int f14 (int x, int q);
#pragma omp declare variant (f3) match (construct={simd(inbranch simdlen (4) linear (q:1))})		/* { dg-error "clauses in 'simd' trait should be separated by ','" } */
int f15 (int x, int q);
#pragma omp declare variant (f3) match (construct={simd(inbranch, simdlen (5-1) linear (q:4-3))})	/* { dg-error "clauses in 'simd' trait should be separated by ','" } */
int f16 (int x, int q);
