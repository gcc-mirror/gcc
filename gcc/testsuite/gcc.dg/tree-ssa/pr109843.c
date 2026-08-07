/* PR tree-optimization/109843 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#include <stdbool.h>

/* Transforms to copysign (y, x)  */

float copysign1 (float x, float y)
{
  bool t = __builtin_signbit (x) == 0;
  bool t1 = __builtin_signbit (y) == 0;
  return (t == t1) ? y : -y;
}

float copysign2 (float x, float y)
{
  bool t = __builtin_signbit (x) != 0;
  bool t1 = __builtin_signbit (y) != 0;
  return (t == t1) ? y : -y;
}

float copysign3 (float x, float y)
{
  bool t = __builtin_signbit (x) != 0;
  bool t1 = __builtin_signbit (y) == 0;
  return (t != t1) ? y : -y;
}

float copysign4 (float x, float y)
{
  bool t = __builtin_signbit (x) == 0;
  bool t1 = __builtin_signbit (y) != 0;
  return (t != t1) ? y : -y;
}

float copysign5 (float x, float y)
{
  bool t = __builtin_signbit (y) == 0;
  bool t1 = __builtin_signbit (x) == 0;
  return (t == t1) ? y : -y;
}

/* Transforms to copysign (y, -x)  */

float copysign6 (float x, float y)
{
  bool t = __builtin_signbit (x) == 0;
  bool t1 = __builtin_signbit (y) == 0;
  return (t != t1) ? y : -y;
}

float copysign7 (float x, float y)
{
  bool t = __builtin_signbit (x) != 0;
  bool t1 = __builtin_signbit (y) == 0;
  return (t == t1) ? y : -y;
}

float copysign8 (float x, float y)
{
  bool t = __builtin_signbit (x) == 0;
  bool t1 = __builtin_signbit (y) != 0;
  return (t == t1) ? y : -y;
}

float copysign9 (float x, float y)
{
  bool t = __builtin_signbit (x) != 0;
  bool t1 = __builtin_signbit (y) != 0;
  return (t != t1) ? y : -y;
}

/* { dg-final { scan-tree-dump-not "signbit" "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "= \\.COPYSIGN" 9 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times " = -" 4 "optimized" { target ifn_copysign } } } */
