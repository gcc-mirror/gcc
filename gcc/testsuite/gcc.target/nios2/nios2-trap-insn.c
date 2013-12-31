/* { dg-do compile } */
/* { dg-final { scan-assembler "break\\t3" } } */

/* Test the nios2 trap instruction */
void foo(void){
  __builtin_trap();
}
