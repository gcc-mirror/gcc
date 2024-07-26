/* PR target/116065  */
/* { dg-do compile } */
/* { dg-options "-O1 -mno-avx" } */

#ifndef __AVX__
#pragma GCC push_options
#pragma GCC target("avx")
#define __DISABLE_AVX__
#endif /* __AVX__ */

extern inline double __attribute__((__gnu_inline__,__always_inline__))
     foo (double x) { return x; }

#ifdef __DISABLE_AVX__
#undef __DISABLE_AVX__
#pragma GCC pop_options
#endif /* __DISABLE_AVX__ */

void __attribute__((target ("avx"), optimize(3)))
bar (double *p)
{
  *p = foo (*p);
}

