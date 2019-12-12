#include <arm_sve.h>

void
f (svint8_t s8, svuint16_t u16, svfloat32_t f32,
   svint16x2_t s16x2, svuint32x3_t u32x3, svfloat64x4_t f64x4,
   svbool_t pg)
{
  s8 = no_ret_s8 (); /* { dg-error {incompatible types when assigning to type 'svint8_t' from type 'int'} } */
  u16 = no_ret_u16 (); /* { dg-error {incompatible types when assigning to type 'svuint16_t' from type 'int'} } */
  f32 = no_ret_f32 (); /* { dg-error {incompatible types when assigning to type 'svfloat32_t' from type 'int'} } */
  s16x2 = no_ret_s16x2 (); /* { dg-error {incompatible types when assigning to type 'svint16x2_t' from type 'int'} } */
  u32x3 = no_ret_u32x3 (); /* { dg-error {incompatible types when assigning to type 'svuint32x3_t' from type 'int'} } */
  f64x4 = no_ret_f64x4 (); /* { dg-error {incompatible types when assigning to type 'svfloat64x4_t' from type 'int'} } */
  pg = no_ret_pg (); /* { dg-error {incompatible types when assigning to type 'svbool_t' from type 'int'} } */

  no_pass_args (pg, u16, f32, s16x2, u32x3, f64x4, pg);
}
