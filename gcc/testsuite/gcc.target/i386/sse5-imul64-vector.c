/* Test that the compiler properly optimizes floating point multiply and add
   instructions vector into pmacsdd/etc. on SSE5 systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -msse5 -ftree-vectorize" } */

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

/* { dg-final { scan-assembler "pmacsdd" } } */
/* { dg-final { scan-assembler "phadddq" } } */
/* { dg-final { scan-assembler "pmacsdql" } } */
