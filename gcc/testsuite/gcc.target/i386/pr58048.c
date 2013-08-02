/* PR target/58048 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
div3 (void)
{
    double tmp1;

    asm volatile ("fscale":"=t" (tmp1):"0" (0), "u" (0)); /* { dg-error "inconsistent operand constraints in an 'asm'"  } */
}
