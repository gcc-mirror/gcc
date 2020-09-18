/* { dg-do compile } */

void
foo (void)
{
  char reg = 10;
  __asm__ ("mov.b %d1, %0\n" : "=r" (reg) : "r" (reg));
  /* { dg-error "invalid 'asm': %d, %e, %f, %g operand modifiers are for memory references or constant values only" "" { target *-*-* } .-1 } */
  __asm__ ("mov.b %e1, %0\n" : "=r" (reg) : "r" (reg));
  /* { dg-error "invalid 'asm': %d, %e, %f, %g operand modifiers are for memory references or constant values only" "" { target *-*-* } .-1 } */
  __asm__ ("mov.b %f1, %0\n" : "=r" (reg) : "r" (reg));
  /* { dg-error "invalid 'asm': %d, %e, %f, %g operand modifiers are for memory references or constant values only" "" { target *-*-* } .-1 } */
  __asm__ ("mov.b %g1, %0\n" : "=r" (reg) : "r" (reg));
  /* { dg-error "invalid 'asm': %d, %e, %f, %g operand modifiers are for memory references or constant values only" "" { target *-*-* } .-1 } */
}
