/* { dg-do compile } */
/* { dg-require-effective-target cv_alu } */
/* { dg-options "-march=rv32i_xcvalu -mabi=ilp32" } */

extern int d;

void
foo(int a, int b)
{
  d = __builtin_riscv_cv_alu_addN (a, b, 65536); /* { dg-warning "unsigned conversion from \'int\' to \'unsigned char\' changes value from \'65536\' to \'0\'" } */
}
