/* Test that the compiler properly optimizes vector 32-bit integer point
   multiply and add instructions vector into pmacsdd on SSE5 systems.  */

/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -msse5 -ftree-vectorize" } */

extern void exit (int);

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));

#define SIZE 10240

union {
  __m128i align;
  int i[SIZE];
} a, b, c, d;

void
int_mul_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.i[i] = (b.i[i] * c.i[i]) + d.i[i];
}

int main ()
{
  int_mul_add ();
  exit (0);
}

/* { dg-final { scan-assembler "pmacsdd" } } */
