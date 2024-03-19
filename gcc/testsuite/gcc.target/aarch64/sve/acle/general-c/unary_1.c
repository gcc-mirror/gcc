/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint32_t s32, svuint32_t u32, svfloat32_t f32)
{
  svabs_m (s32, pg); /* { dg-error {too few arguments to function 'svabs_m'} } */
  svabs_m (s32, pg, s32, s32); /* { dg-error {too many arguments to function 'svabs_m'} } */
  svabs_m (0, pg, s32); /* { dg-error {passing 'int' to argument 1 of 'svabs_m', which expects an SVE type rather than a scalar} } */
  svabs_m (s32, s32, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svabs_m', which expects 'svbool_t'} } */
  svabs_m (s32, 0, s32); /* { dg-error {passing 'int' to argument 2 of 'svabs_m', which expects 'svbool_t'} } */
  svabs_m (s32, pg, s32);
  svabs_m (u32, pg, u32); /* { dg-error {'svabs_m' has no form that takes 'svuint32_t' arguments} } */
  svabs_m (f32, pg, f32);
  svabs_m (s32, pg, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svabs_m', but argument 1 had type 'svint32_t'} } */
  svabs_m (s32, pg, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svabs_m', but argument 1 had type 'svint32_t'} } */
  svabs_m (s32, pg, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svabs_m', but argument 1 had type 'svint32_t'} } */
  svabs_m (pg, pg, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svabs_m', but argument 1 had type 'svbool_t'} } */
  svabs_m (pg, pg, pg); /* { dg-error {'svabs_m' has no form that takes 'svbool_t' arguments} } */
}
