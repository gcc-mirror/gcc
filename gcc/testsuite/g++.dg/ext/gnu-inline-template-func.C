/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler "func4" } } */
/* { dg-final { scan-assembler-not "func5" } } */

#define defpfx template <typename T>

#include "gnu-inline-global.C"

template int func1<int>(void);
template int func2<int>(void);
template int func3<int>(void);
template int func4<int>(void);
template int func5<int>(void);
