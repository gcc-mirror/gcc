/* PR rtl-optimization/87600  */
/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* s390*-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

#include "pr87600.h"

/* The following are all invalid uses of local register variables.  */

long
test0 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG1);
  asm ("blah %0 %1" : "=r" (var1), "=r" (var2)); /* { dg-error "invalid hard register usage between output operands" } */
  return var1;
}

long
test1 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG2);
  asm ("blah %0 %1" : "=r" (var1) : "0" (var2)); /* { dg-error "invalid hard register usage between output operand and matching constraint operand" } */
  return var1;
}

long
test2 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG1);
  asm ("blah %0 %1" : "=&r" (var1) : "r" (var2)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
  return var1;
}

long
test3 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG1);
  long var3;
  asm ("blah %0 %1" : "=&r" (var1), "=r" (var3) : "1" (var2)); /* { dg-error "invalid hard register usage between earlyclobber operand and input operand" } */
  return var1 + var3;
}
