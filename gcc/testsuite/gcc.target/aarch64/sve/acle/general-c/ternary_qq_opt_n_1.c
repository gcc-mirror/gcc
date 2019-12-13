/* { dg-do compile } */

#include <arm_sve.h>

svint32_t
f1 (svuint32_t u32, svuint8_t u8, svint8_t s8)
{
  svdot_u32 (u32); /* { dg-error {too few arguments to function 'svdot_u32'} } */
  svdot_u32 (u32, u8, u8, u32); /* { dg-error {too many arguments to function 'svdot_u32'} } */
  svdot_u32 (u32, u32, u8); /* { dg-error {incompatible type for argument 2 of 'svdot_u32'} } */
  svdot_u32 (u32, s8, u8); /* { dg-error {incompatible type for argument 2 of 'svdot_u32'} } */
  svdot_u32 (u32, u8, u32); /* { dg-error {incompatible type for argument 3 of 'svdot_u32'} } */
  svdot_u32 (u32, u8, s8); /* { dg-error {incompatible type for argument 3 of 'svdot_u32'} } */
  return svdot_u32 (u32, u8, u8); /* { dg-error {incompatible types when returning type 'svuint32_t' but 'svint32_t' was expected} } */
}
