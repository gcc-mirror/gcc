/* { dg-do compile } */
/* { dg-final { scan-assembler "trap\\t3|trap.n\\t3" } } */

/* Test the nios2 trap instruction */
void foo(void){
  __builtin_trap();
}
