/* { dg-do compile } */
/* { dg-require-effective-target cv_alu } */
/* { dg-options "-march=rv32i_xcvalu -mabi=ilp32" } */

extern int d;

void
foo(int a, int b)
{
  d = __builtin_riscv_cv_alu_clipu (a, 4294967296); /* { dg-warning "unsigned conversion from \'long long int\' to \'unsigned int\' changes value from \'4294967296\' to \'0\'" } */
}
