/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler "wrctl\\tctl6, zero" } } */

void foo(void){
  __builtin_wrctl(6,0);
}
