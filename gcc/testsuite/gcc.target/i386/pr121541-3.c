/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only" } */

extern long double d;

__attribute__ ((target("80387")))
void
func1 (void)
{
  d *= 3;
}
