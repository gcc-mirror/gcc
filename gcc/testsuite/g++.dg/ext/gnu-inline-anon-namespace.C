/* { dg-do compile } */
/* { dg-options "-O" } */ // such that static functions are optimized out
/* { dg-final { scan-assembler-not "func1" } } */
/* { dg-final { scan-assembler-not "func2" } } */
/* { dg-final { scan-assembler-not "func3" } } */
/* { dg-final { scan-assembler-not "func4" } } */
/* { dg-final { scan-assembler-not "func5" } } */

namespace {
#include "gnu-inline-global.C"
}
