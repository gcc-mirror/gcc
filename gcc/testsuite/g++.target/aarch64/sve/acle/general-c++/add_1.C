/* { dg-do compile } */

#include "add_1.h"

svuint8_t
f1 (svbool_t pg, svuint8_t x, svint8_t y)
{
  return svadd_u8_x (pg, x, y); /* { dg-error "cannot convert 'svint8_t' to 'svuint8_t'" } */
}
