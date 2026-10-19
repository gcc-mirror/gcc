/* PR target/127957 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-mgeneral-regs-only -mf16c" } */

double foo() { return __builtin_nansf16("nan"); }
