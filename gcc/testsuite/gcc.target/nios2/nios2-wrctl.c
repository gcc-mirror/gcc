/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-final { scan-assembler "wrctl" } } */

void foo(void){
  __builtin_wrctl(6,4);
}
