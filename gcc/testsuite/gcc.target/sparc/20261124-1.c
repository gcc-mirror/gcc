/* PR target/128992 */

/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-std=gnu99" } */

extern void abort (void);

/* Pass a 12-byte structure partially in slot #15 and on the stack.  */

struct S1 { float f1; _Complex float f2; };

void f1 (int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8,
	 int p9, int p10, int p11, int p12, int p13, int p14, int p15,
	 struct S1 s1)
{
  register float f30 asm ("f30");
  register float f31 asm ("f31");

  if (f30 != 4.0f)
    abort ();

  if (f31 != 5.0f)
    abort ();

  if (s1.f1 != 4.0f)
    abort ();

  if (__real__ s1.f2 != 5.0f)
    abort ();

  if (__imag__ s1.f2 != 6.0f)
    abort ();
}

int main (void)
{
  struct S1 s1 = { 4.0f, 5.0f + 6.0fi };

  f1 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, s1);

  return 0;
}
