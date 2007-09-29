/* Test that the compiler properly optimizes vector SI->DI conversions.  This
   was a bug in the initial SSE5 code.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -msse5 -ftree-vectorize" } */

typedef long long __m128i  __attribute__ ((__vector_size__ (16), __may_alias__));

#define SIZE 10240
union {
  signed   int   si[SIZE];
  signed   long  sl[SIZE];
  __m128i        align;
} a, b;

void conv_sign_int_sign_long (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.sl[i] = b.si[i];
}

/* { dg-final { scan-assembler "pperm" } } */
