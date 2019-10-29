/* { dg-do compile } */

#include "dot_2.h"

void
f1 (svuint32_t x, svint8_t y, svuint8_t z)
{
  svdot (x, y); /* { dg-error {no matching function for call to 'svdot\(svuint32_t&, svint8_t&\)'} } */
  svdot (x, x, x); /* { dg-error {no matching function for call to 'svdot\(svuint32_t&, svuint32_t&, svuint32_t&\)'} } */
  svdot (1, z, z); /* { dg-error {no matching function for call to 'svdot\(int, svuint8_t&, svuint8_t&\)'} } */
  svdot (x, y, z); /* { dg-error {no matching function for call to 'svdot\(svuint32_t&, svint8_t&, svuint8_t&\)'} } */
}
