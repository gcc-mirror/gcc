/* Test that the compiler properly optimizes vector shift instructions into
   psha/pshl on XOP systems.  */

/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mxop -mno-avx2 -ftree-vectorize" } */

extern void exit (int);

typedef long __m128i  __attribute__ ((__vector_size__ (16), __may_alias__));

#define SIZE 10240

union {
  __m128i i_align;
  int i32[SIZE];
  unsigned u32[SIZE];
} a, b, c;

void
right_sign_shift32 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a.i32[i] = b.i32[i] >> c.i32[i];
}

int main ()
{
  right_sign_shift32 ();
  exit (0);
}

/* { dg-final { scan-assembler "vpshad" } } */
