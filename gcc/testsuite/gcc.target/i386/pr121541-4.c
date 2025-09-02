/* { dg-do compile } */
/* { dg-options "-O2" } */

extern long double d;

__attribute__ ((target("general-regs-only","80387")))
void
func1 (void)
{
  d *= 3;
}
