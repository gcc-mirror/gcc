/* { dg-do compile } */

/* There's no requirement to diagnose this.  In particular, arm_sve.h
   is allowed to use macros to implement the functions, and defining
   a macro that matches an existing symbol would not be diagnosed.

   At the moment this works like other built-ins in the sense that the
   explicit definition "wins".  This isn't supported behavior though.  */
__SVUint8_t
svadd_u8_x (__SVBool_t pg, __SVUint8_t x, __SVUint8_t y)
  __arm_streaming_compatible
{
  return x;
}

#pragma GCC aarch64 "arm_sve.h"

svuint8_t
f (svbool_t pg, svuint8_t x, svuint8_t y)
{
  return svadd_u8_x (pg, x, y);
}
