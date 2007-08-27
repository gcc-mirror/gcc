/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler "func4" } } */
/* { dg-final { scan-assembler "func5" } } */

#define IN_CLASS gnu_test

struct IN_CLASS {
  int func1(void);
  int func2(void);
  int func3(void);
  int func4(void);
  int func5(void);
};

#include "gnu-inline-global.C"
