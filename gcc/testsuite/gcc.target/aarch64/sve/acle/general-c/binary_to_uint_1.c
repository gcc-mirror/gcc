/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

svuint8_t
f1 (svbool_t pg, svint32_t s32, svuint32_t u32)
{
  svhistcnt_z (pg, s32); /* { dg-error {too few arguments to function 'svhistcnt_z'} } */
  svhistcnt_z (pg, s32, s32, 0); /* { dg-error {too many arguments to function 'svhistcnt_z'} } */
  svhistcnt_z (0, s32, s32); /* { dg-error {passing 'int' to argument 1 of 'svhistcnt_z', which expects 'svbool_t'} } */
  svhistcnt_z (s32, s32, s32); /* { dg-error {passing 'svint32_t' to argument 1 of 'svhistcnt_z', which expects 'svbool_t'} } */
  svhistcnt_z (pg, 0, s32); /* { dg-error {passing 'int' to argument 2 of 'svhistcnt_z', which expects an SVE vector type} } */
  svhistcnt_z (pg, pg, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svhistcnt_z', but previous arguments had type 'svbool_t'} } */
  svhistcnt_z (pg, s32, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svhistcnt_z', but previous arguments had type 'svint32_t'} } */
  svhistcnt_z (pg, s32, 0); /* { dg-error {passing 'int' to argument 3 of 'svhistcnt_z', which expects an SVE vector type} } */
  svhistcnt_z (pg, pg, pg); /* { dg-error {'svhistcnt_z' has no form that takes 'svbool_t' arguments} } */
}
