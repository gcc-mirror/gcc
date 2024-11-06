/* PR tree-optimization/116024 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details" } */

#include <stdint.h>

uint32_t f(void);

int32_t i3(void)
{
  uint32_t l = 10 + (uint32_t)f();
  return l <= 9; // f() >= -10u
}

int32_t i3a(void)
{
  uint32_t l = 20 + (uint32_t)f();
  return l < 20; // f() > -21u
}

int32_t i3b(void)
{
  uint32_t l = 30 + (uint32_t)f();
  return l >= 30; // f() <= -31u
}

int32_t i3c(void)
{
  uint32_t l = 40 + (uint32_t)f();
  return l > 39; // f() < -39u
}

/* { dg-final { scan-tree-dump-times "Removing dead stmt:.*? \\+" 4 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* > 4294967285" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* > 4294967275" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* <= 4294967265" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* <= 4294967255" 1 "forwprop1" } } */
