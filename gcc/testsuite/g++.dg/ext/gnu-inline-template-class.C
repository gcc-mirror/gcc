/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler "func4" } } */
/* { dg-final { scan-assembler "func5" } } */

template <typename T> struct gnu_test {
  int func1(void);
  int func2(void);
  int func3(void);
  int func4(void);
  int func5(void);
};

#define defpfx template <typename T>
#define IN_CLASS gnu_test<T>

#include "gnu-inline-global.C"

template struct gnu_test<int>;
