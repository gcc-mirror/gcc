/* PR tree-optimization/116024 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details -fwrapv" } */

#include <stdint.h>
#include <limits.h>

uint32_t f(void);

int32_t i3(void)
{
  int32_t l = -10 + (int32_t)f();
  return l <= INT32_MAX - 10; // f() >= INT32_MIN + 10
}

int32_t i3a(void)
{
  int32_t l = -20 + (int32_t)f();
  return l < INT32_MAX - 19; // f() > INT32_MAX + 20
}

int32_t i3b(void)
{
  int32_t l = 30 + (int32_t)f();
  return l >= INT32_MIN + 30; // f() <= INT32_MAX - 30
}

int32_t i3c(void)
{
  int32_t l = 40 + (int32_t)f();
  return l > INT32_MIN + 39; // f() < INT32_MIN - 40
}

/* { dg-final { scan-tree-dump-times "Removing dead stmt:.*? \\+" 4 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* >= -2147483638" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* >= -2147483628" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* <= 2147483617" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* <= 2147483607" 1 "forwprop1" } } */
