/* Check that -fstack-protector is accepted and that the canary is set from
   __stack_chk_guard and tested before returning.  The scratch registers used
   to carry the canary must be cleared afterwards, so that its value is not
   left recoverable in a register or in a spill slot.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all" } */

extern void g (char *);

int
f (void)
{
  char buf[64];
  g (buf);
  return 0;
}

/* { dg-final { scan-assembler "__stack_chk_guard" } } */
/* { dg-final { scan-assembler "__stack_chk_fail" } } */
/* { dg-final { scan-assembler "\\sxor\\s\\\$\[0-9\]+,\\\$\[0-9\]+,\\\$\[0-9\]+\\s" } } */
/* One scratch cleared by stack_protect_setdi, one by stack_protect_testdi.
   The pattern deliberately does not match "bis $31,$31,$31", which is a nop
   the scheduler may insert anywhere.  */
/* { dg-final { scan-assembler-times "\\sbis\\s\\\$31,\\\$31,\\\$(\[0-9\]|1\[0-9\]|2\[0-9\]|30)\\n" 2 } } */
