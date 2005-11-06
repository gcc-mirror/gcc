/* { dg-do compile } */
/* { dg-options "-O2 -mmmx" } */
/* { dg-final { scan-assembler-not "paddb" } } */

typedef int v2si __attribute__ ((__vector_size__ (8)));
typedef short v4hi __attribute__ ((__vector_size__ (8)));
typedef char v8qi __attribute__ ((__vector_size__ (8)));

int
foo (unsigned int *a, unsigned int *b)
{
  long long i, j, k;

  i = (long long) __builtin_ia32_vec_init_v2si (*a, 0);
  j = (long long) __builtin_ia32_vec_init_v2si (*b, 0);
  i = (long long) __builtin_ia32_punpcklbw ((v8qi) i, (v8qi) 0ll);
  j = (long long) __builtin_ia32_punpcklbw ((v8qi) j, (v8qi) 0ll);
  k = (long long) __builtin_ia32_paddw ((v4hi) i, (v4hi) j);
  return __builtin_ia32_vec_ext_v2si ((v2si) k, 0);
}
