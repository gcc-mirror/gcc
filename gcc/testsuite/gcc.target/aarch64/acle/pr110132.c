/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.7-a" } */

/* Check that ls64 builtins can be invoked using a preprocesed testcase
   without triggering bogus builtin warnings, see PR110132.

   Note that this is purely to test GCC internals and user code should
   include arm_acle.h to make use of these builtins.  */

#pragma GCC aarch64 "arm_acle.h"
typedef __arm_data512_t data512_t;
void f(void *p, data512_t d)
{
  __arm_st64b (p, d);
}
