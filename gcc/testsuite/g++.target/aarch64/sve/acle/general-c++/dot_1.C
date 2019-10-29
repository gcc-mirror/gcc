/* { dg-do compile } */

#include "dot_1.h"

svuint32_t
f1 (svuint32_t x, svint8_t y, svuint8_t z)
{
  return svdot_u32 (x, y, z); /* { dg-error "cannot convert 'svint8_t' to 'svuint8_t'" } */
}
