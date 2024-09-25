/* { dg-do compile } */

long double
test (long double xx)
{
   __asm ("" :: "f"(xx)); /* { dg-error "inconsistent operand constraints in an 'asm'" } */
   return xx + 1;
}
