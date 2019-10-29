/* { dg-do compile } */

#include "add_2.h"

void
f1 (svbool_t pg, svuint8_t x, svint8_t y)
{
  svadd_x (pg, x); /* { dg-error {no matching function for call to 'svadd_x\(svbool_t&, svuint8_t&\)'} } */
  svadd_x (pg, x, x, x); /* { dg-error {no matching function for call to 'svadd_x\(svbool_t&, svuint8_t&, svuint8_t&, svuint8_t&\)'} } */
  svadd_x (x, x, x); /* { dg-error {no matching function for call to 'svadd_x\(svuint8_t&, svuint8_t&, svuint8_t&\)'} } */
  svadd_x (pg, pg, pg); /* { dg-error {no matching function for call to 'svadd_x\(svbool_t&, svbool_t&, svbool_t&\)'} } */
  svadd_x (pg, 1, x); /* { dg-error {no matching function for call to 'svadd_x\(svbool_t&, int, svuint8_t&\)'} } */
  svadd_x (pg, x, y); /* { dg-error {no matching function for call to 'svadd_x\(svbool_t&, svuint8_t&, svint8_t&\)'} } */
}
