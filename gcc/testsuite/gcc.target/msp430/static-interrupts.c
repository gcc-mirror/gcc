/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-times "__interrupt_vector_" 4 } } */

/* Test that interrupts aren't optimised out and that "__interrupt__" and
   "interrupt" can be used interchangeably.  */

static void __attribute__((interrupt(1)))
isr_static (void)
{
}

static void __attribute__((__interrupt__(2)))
isr_static_alt (void)
{
}

void __attribute__((interrupt(3)))
isr_global (void)
{
}

void __attribute__((__interrupt__(4)))
isr_global_alt (void)
{
}
