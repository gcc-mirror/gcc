/* Test that the compiler properly optimizes floating point multiply and add
   instructions vector into pmacsdd/etc. on XOP systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mxop -ftree-vectorize" } */

extern void exit (int);

typedef long __m128i  __attribute__ ((__vector_size__ (16), __may_alias__));

#define SIZE 10240

union {
  __m128i i_align;
  int i32[SIZE];
  long i64[SIZE];
} a, b, c, d;

void
imul32_to_64 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.i64[i] = ((long)b.i32[i]) * ((long)c.i32[i]);
}

int main ()
{
  imul32_to_64 ();
  exit (0);
}

/* { dg-final { scan-assembler "vpmuldq" } } */
/* { dg-final { scan-assembler "vpmacsdqh" } } */
