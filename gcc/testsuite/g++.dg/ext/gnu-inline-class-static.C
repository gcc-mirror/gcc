/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler "func4" } } */
/* { dg-final { scan-assembler "func5" } } */

#undef IN_CLASS
#define IN_CLASS gnu_test_static

struct IN_CLASS {
  static int func1(void);
  static int func2(void);
  static int func3(void);
  static int func4(void);
  static int func5(void);
};

#include "gnu-inline-global.C"
