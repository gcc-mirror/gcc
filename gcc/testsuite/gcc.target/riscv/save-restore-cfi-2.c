/* { dg-do compile } */
/* { dg-options "-fdump-rtl-pro_and_epilogue -O2 -march=rv64gc -mabi=lp64d -msave-restore -mcmodel=medany" } */
/* { dg-skip-if "" { *-*-* } {"-Os" "-O1" "-O0" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { scan-rtl-dump {expr_list:REG_CFA_OFFSET \(set \(mem/c:DI} "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump {expr_list:REG_CFA_RESTORE \(reg:DI 8 s0\)} "pro_and_epilogue" } } */

char my_getchar();
float getf();

int foo()
{
  int s0 = my_getchar();
  float f0 = getf();
  int b = my_getchar();
  return f0 + s0 + b;
}
