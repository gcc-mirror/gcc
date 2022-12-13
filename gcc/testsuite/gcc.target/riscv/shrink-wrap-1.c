/* { dg-do compile } */
/* { dg-options "-fshrink-wrap" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" } } */

void g(void);

void f(int x)
{
  if (x)
    {
      /* Force saving of some callee-saved registers. With shrink wrapping
	 enabled these only need to be saved if x is non-zero.  */
      register int s2 asm("18") = x;
      register int s3 asm("19") = x;
      register int s4 asm("20") = x;
      asm("" : : "r"(s2));
      asm("" : : "r"(s3));
      asm("" : : "r"(s4));
      g();
    }
}

/* The resulting code should do nothing if X is 0.  */
/* { dg-final { scan-assembler "bne\ta0,zero,.*\n.*ret" } } */
