/* PR rtl-optimization/87600  */
/* { dg-do compile { target aarch64*-*-* arm*-*-* i?86-*-* powerpc*-*-* s390*-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

#include "pr87600.h"

/* The following are all valid uses of local register variables.  */

long
test0 (long arg)
{
  register long var asm (REG1);
  asm ("blah %0 %1" : "+&r" (var) : "r" (arg));
  return var;
}

long
test1 (long arg0, long arg1)
{
  register long var asm (REG1);
  asm ("blah %0, %1, %2" : "=&r" (var) : "r" (arg0), "0" (arg1));
  return var + arg1;
}

long
test2 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG1);
  asm ("blah %0 %1" : "=&r" (var1) : "0" (var2));
  return var1;
}

long
test3 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG2);
  long var3;
  asm ("blah %0 %1" : "=&r" (var1), "=r" (var3) : "1" (var2));
  return var1 + var3;
}

long
test4 (void)
{
  register long var1 asm (REG1);
  register long var2 asm (REG2);
  register long var3 asm (REG2);
  asm ("blah %0 %1" : "=&r" (var1), "=r" (var2) : "1" (var3));
  return var1;
}
