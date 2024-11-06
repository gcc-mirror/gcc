/* PR tree-optimization/116024 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details -fwrapv" } */

#include <stdint.h>

uint32_t f(void);

int32_t i2(void)
{
  int32_t l = 10 - (int32_t)f();
  return l <= 20; // f() - 11 >= -21
}

int32_t i2a(void)
{
  int32_t l = 10 - (int32_t)f();
  return l < 30; // f() - 11 > -31
}

int32_t i2b(void)
{
  int32_t l = 200 - (int32_t)f();
  return l <= 100; // f() - 201 >= -101
}

int32_t i2c(void)
{
  int32_t l = 300 - (int32_t)f();
  return l < 100; // f() - 301 > -101
}

int32_t i2d(void)
{
  int32_t l = 1000 - (int32_t)f();
  return l >= 2000; // f() - 1001 <= -2001
}

int32_t i2e(void)
{
  int32_t l = 1000 - (int32_t)f();
  return l > 3000; // f() - 1001 < -3001
}

int32_t i2f(void)
{
  int32_t l = 20000 - (int32_t)f();
  return l >= 10000; // f() - 20001 <= -10001
}

int32_t i2g(void)
{
  int32_t l = 30000 - (int32_t)f();
  return l > 10000; // f() - 30001 < -10001
}

/* { dg-final { scan-tree-dump-times "Removing dead stmt:.*?- _" 8 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -11.*\n.*>= -21" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -11.*\n.*>= -30" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -201.*\n.*>= -101" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -301.*\n.*>= -100" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -1001.*\n.*< -2000" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -1001.*\n.*< -3001" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -20001.*\n.*< -10000" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ -30001.*\n.*< -10001" 1 "forwprop1" } } */
