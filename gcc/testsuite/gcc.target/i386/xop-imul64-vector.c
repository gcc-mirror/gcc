/* Test that the compiler properly optimizes floating point multiply and add
   instructions vector into pmacsdd/etc. on XOP systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mxop -mno-avx2 -ftree-vectorize" } */

extern void exit (int);

typedef long __m128i  __attribute__ ((__vector_size__ (16), __may_alias__));

#define SIZE 10240

union {
  __m128i i_align;
  long i64[SIZE];
} a, b, c, d;

void
imul64 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.i64[i] = b.i64[i] * c.i64[i];
}

int main ()
{
  imul64 ();
  exit (0);
}

/* { dg-final { scan-assembler "vpmulld" } } */
/* { dg-final { scan-assembler "vphadddq" } } */
/* { dg-final { scan-assembler "vpmacsdql" } } */
