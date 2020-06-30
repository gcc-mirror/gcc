/* { dg-do compile } */
/* { dg-skip-if "" { ! arm_cortex_m } { "-mthumb" } } */
/* { dg-options "-mgeneral-regs-only" } */

/* Below code used to trigger an ICE due to missing constraints for
   sp = fp + cst pattern.  */

void fiq_handler (void) __attribute__((interrupt ("FIQ")));

void
fiq_handler (void)
{
}
