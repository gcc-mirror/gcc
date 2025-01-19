/* PR middle-end/114540 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo ()
{
  asm ("" : : : "2147483648");			/* { dg-error "unknown register name" } */
  asm ("" : : : "4294967296");			/* { dg-error "unknown register name" } */
  asm ("" : : : "18446744073709551616");	/* { dg-error "unknown register name" } */
  asm ("" : : : "9223372036854775808");		/* { dg-error "unknown register name" } */
  asm ("" : : : "9223372036854775807");		/* { dg-error "unknown register name" } */
  asm ("" : : : "2147483647");			/* { dg-error "unknown register name" } */
  asm ("" : : : "4&");				/* { dg-error "unknown register name" } */
  asm ("" : : : "1'0");				/* { dg-error "unknown register name" } */
#if defined(__i386__) || defined(__x86_64__) || defined(__powerpc__) || defined(__s390__) || defined(__aarch64__) || defined(__arm__)
  asm ("" : : : "0");
#endif

  register int a asm("2147483648");		/* { dg-error "invalid register name for" } */
  register int b asm("4294967296");		/* { dg-error "invalid register name for" } */
  register int c asm("18446744073709551616");	/* { dg-error "invalid register name for" } */
  register int d asm("9223372036854775808");	/* { dg-error "invalid register name for" } */
  register int e asm("9223372036854775807");	/* { dg-error "invalid register name for" } */
  register int f asm("2147483647");		/* { dg-error "invalid register name for" } */
  register int g asm("4&");			/* { dg-error "invalid register name for" } */
  register int h asm("1'0");			/* { dg-error "invalid register name for" } */
#if defined(__i386__) || defined(__x86_64__) || defined(__powerpc__) || defined(__s390__) || defined(__aarch64__) || defined(__arm__)
  register int i asm("0");
#endif
}
