/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler "func1" } } */
/* { dg-final { scan-assembler "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler "func4" } } */
/* { dg-final { scan-assembler-not "func5" } } */

namespace gnu_test {
#include "gnu-inline-global.C"
}
