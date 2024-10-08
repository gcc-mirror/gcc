/* PR tree-optimization/116024 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details" } */

#include <stdint.h>

uint32_t f(void);

int32_t i2(void)
{
  uint32_t l = 10 - (uint32_t)f();
  return l <= 20; // f() + 10 <= 20 
}

int32_t i2a(void)
{
  uint32_t l = 10 - (uint32_t)f();
  return l < 30; // f() + 19 < 30 
}

int32_t i2b(void)
{
  uint32_t l = 200 - (uint32_t)f();
  return l <= 100; // f() - 100 <= 100 
}

int32_t i2c(void)
{
  uint32_t l = 300 - (uint32_t)f();
  return l < 100; // f() - 201 < 100
}

int32_t i2d(void)
{
  uint32_t l = 1000 - (uint32_t)f();
  return l >= 2000; // f() + 999 >= 2000
}

int32_t i2e(void)
{
  uint32_t l = 1000 - (uint32_t)f();
  return l > 3000; // f() + 2000 > 3000
}

int32_t i2f(void)
{
  uint32_t l = 20000 - (uint32_t)f();
  return l >= 10000; // f() - 10001 >= 10000
}

int32_t i2g(void)
{
  uint32_t l = 30000 - (uint32_t)f();
  return l > 10000; // f() - 20000 > 10000
}

/* { dg-final { scan-tree-dump-times "Removing dead stmt:.*?- _" 8 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 10.*\n.*<= 20" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 19.*\n.*<= 29" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 4294967196.*\n.*<= 100" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 4294967095.*\n.*<= 99" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 999.*\n.*> 1999" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 2000.*\n.*> 3000" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 4294957295.*\n.*> 9999" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* \\+ 4294947296.*\n.*> 10000" 1 "forwprop1" } } */
