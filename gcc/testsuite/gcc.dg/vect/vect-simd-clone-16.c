/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd --param vect-epilogues-nomask=0" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

/* Test that simd inbranch clones work correctly.  */

#ifndef TYPE
#define TYPE int
#endif

/* A simple function that will be cloned.  */
#pragma omp declare simd inbranch
TYPE __attribute__((noinline))
foo (TYPE a)
{
  return a + 1;
}

/* Check that "inbranch" clones are called correctly.  */

void __attribute__((noipa))
masked (TYPE * __restrict a, TYPE * __restrict b, int size)
{
  #pragma omp simd
  for (int i = 0; i < size; i++)
    b[i] = a[i]<1 ? foo(a[i]) : a[i];
}

/* Check that "inbranch" works when there might be unrolling.  */

void __attribute__((noipa))
masked_fixed (TYPE * __restrict a, TYPE * __restrict b)
{
  #pragma omp simd
  for (int i = 0; i < 128; i++)
    b[i] = a[i]<1 ? foo(a[i]) : a[i];
}

/* Validate the outputs.  */

void
check_masked (TYPE *b, int size)
{
  for (int i = 0; i < size; i++)
    if (((TYPE)i < 1 && b[i] != (TYPE)(i + 1))
	|| ((TYPE)i >= 1 && b[i] != (TYPE)i))
      {
	__builtin_printf ("error at %d\n", i);
	__builtin_exit (1);
      }
}

int
main ()
{
  TYPE a[1024];
  TYPE b[1024];

  for (int i = 0; i < 1024; i++)
    a[i] = i;

  masked_fixed (a, b);
  check_masked (b, 128);

  /* Test various sizes to cover machines with different vectorization
     factors.  */
  for (int size = 8; size <= 1024; size *= 2)
    {
      masked (a, b, size);
      check_masked (b, size);
    }

  /* Test sizes that might exercise the partial vector code-path.  */
  for (int size = 8; size <= 1024; size *= 2)
    {
      masked (a, b, size-4);
      check_masked (b, size-4);
    }

  return 0;
}

/* Ensure the the in-branch simd clones are used on targets that support them.  */
/* { dg-final { scan-tree-dump-times {[\n\r] [^\n]* = foo\.simdclone} 2 "vect" } } */

/* The LTO test produces two dump files and we scan the wrong one.  */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
