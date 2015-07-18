/* { dg-do run } */
/* { dg-options "-O1 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

struct S 
{
  int:31;
  int:2;
  int f0:16;
  int f1;
  int f2;
};

static void 
sse2_test (void)
{
  struct S a = { 1, 0, 0 };

  if (a.f0 != 1)
    __builtin_abort(); 
}
