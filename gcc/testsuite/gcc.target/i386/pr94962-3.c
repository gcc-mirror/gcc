/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
/* { dg-final { scan-assembler-times "vpcmpeqd\[ \\t\]+%ymm\[0-9\]" 4 } } */

typedef long long __v8di __attribute__ ((__vector_size__ (64)));
typedef int __v16si __attribute__ ((__vector_size__ (64)));
typedef short __v32hi __attribute__ ((__vector_size__ (64)));
typedef char __v64qi __attribute__ ((__vector_size__ (64)));
typedef long long __m512i __attribute__ ((__vector_size__ (64), __may_alias__));

__m512i
__attribute__ ((noinline, noclone))
foo1 ()
{
  return __extension__ (__m512i)(__v8di) { -1, -1, -1, -1,
					   0, 0, 0, 0 };
}

__m512i
__attribute__ ((noinline, noclone))
foo2 ()
{
  return __extension__ (__m512i)(__v16si) { -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    0, 0, 0, 0,
					    0, 0, 0, 0 };
}

__m512i
__attribute__ ((noinline, noclone))
foo3 ()
{
  return __extension__ (__m512i)(__v32hi) { -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0 };
}

__m512i
__attribute__ ((noinline, noclone))
foo4 ()
{
  return __extension__ (__m512i)(__v64qi) { -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0 };
}
