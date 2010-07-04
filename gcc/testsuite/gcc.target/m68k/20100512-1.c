/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector" } */
/* { dg-require-effective-target fstack_protector } */
/* There should be 2 references to __stack_chk_guard in every function.  */
/* { dg-final { scan-assembler-times "__stack_chk_guard" 4 } } */

#include <stdlib.h>
#include <string.h>
void doTest1(void) {
  volatile char foo[10];
  memset((void *)foo, 1, 100);
}
void doTest2(void) {
  volatile char foo[10];
  memset((void *)foo, 1, 100);
}
