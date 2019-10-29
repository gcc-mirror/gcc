/* { dg-do compile } */

__SVUint8_t
svadd_x (__SVBool_t pg, __SVUint8_t x, __SVUint8_t y)
{
  return x;
}

#pragma GCC aarch64 "arm_sve.h"

svuint8_t
f (svbool_t pg, svuint8_t x, svuint8_t y)
{
  return svadd_x (pg, x, y);
}
