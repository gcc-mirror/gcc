/* PR tree-optimization/116024 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details" } */

#include <stdint.h>
#include <limits.h>

uint32_t f(void);

int32_t i1(void)
{
  int32_t l = 10 - (int32_t)f();
  return l <= 9; // f() > 0
}

int32_t i1a(void)
{
  int32_t l = 20 - (int32_t)f();
  return l <= INT32_MIN; // return 0
}

int32_t i1b(void)
{
  int32_t l = 30 - (int32_t)f();
  return l <= INT32_MIN + 31; // f() == INT32_MAX
}

int32_t i1c(void)
{
  int32_t l = INT32_MAX - 40 - (int32_t)f();
  return l <= -38; // f() > INT32_MAX - 3
}

int32_t i1d(void)
{
  int32_t l = INT32_MAX - 50 - (int32_t)f();
  return l <= INT32_MAX - 1; // f() != -50
}

int32_t i1e(void)
{
  int32_t l = INT32_MAX - 60 - (int32_t)f();
  return l != INT32_MAX - 90; // f() != 30
}

int32_t i1f(void)
{
  int32_t l = INT32_MIN + 70 - (int32_t)f();
  return l <= INT32_MAX - 2; // return 0
}

int32_t i1g(void)
{
  int32_t l = INT32_MAX/2 + 30 - (int32_t)f();
  return l <= INT32_MIN/2 - 30; // return 1
}


/* { dg-final { scan-tree-dump-times "Removing dead stmt:.*?- _" 5 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "return 0" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "return 1" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* > 0" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* == 2147483647" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* > 2147483644" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* != 4294967246" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "gimple_simplified to.* != 30" 1 "forwprop1" } } */
