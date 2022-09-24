/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
/* { dg-final { scan-assembler-times "vpcmpeqd\[ \\t\]+%xmm\[0-9\]" 4 } } */

typedef long long __v4di __attribute__ ((__vector_size__ (32)));
typedef int __v8si __attribute__ ((__vector_size__ (32)));
typedef short __v16hi __attribute__ ((__vector_size__ (32)));
typedef char __v32qi __attribute__ ((__vector_size__ (32)));
typedef long long __m256i __attribute__ ((__vector_size__ (32), __may_alias__));

__m256i
__attribute__ ((noinline, noclone))
foo1 ()
{
  return __extension__ (__m256i)(__v4di) { -1, -1, 0, 0 };
}

__m256i
__attribute__ ((noinline, noclone))
foo2 ()
{
  return __extension__ (__m256i)(__v8si) { -1, -1, -1, -1,
					    0, 0, 0, 0 };
}

__m256i
__attribute__ ((noinline, noclone))
foo3 ()
{
  return __extension__ (__m256i)(__v16hi) { -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    0, 0, 0, 0,
					    0, 0, 0, 0 };
}

__m256i
__attribute__ ((noinline, noclone))
foo4 ()
{
  return __extension__ (__m256i)(__v32qi) { -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    -1, -1, -1, -1,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0,
					    0, 0, 0, 0 };
}
