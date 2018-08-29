/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-march=athlon" } */

__float128 a;
void b () { __asm__("" : "+r"(a)); } /* { dg-error "inconsistent operand constraints in an" } */
