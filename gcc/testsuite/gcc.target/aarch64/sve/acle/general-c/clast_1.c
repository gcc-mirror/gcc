#include <arm_sve.h>

void
test (svbool_t pg, svint32_t s32, svint64_t s64, int i)
{
  svclasta (pg, 1); /* { dg-error {too few arguments to function 'svclasta'} } */
  svclasta (pg, 1, s32, 1); /* { dg-error {too many arguments to function 'svclasta'} } */
  svclasta (1, 1, s32); /* { dg-error {passing 'int' to argument 1 of 'svclasta', which expects 'svbool_t'} } */
  svclasta (pg, 1, 1); /* { dg-error {passing 'int' to argument 3 of 'svclasta', which expects an SVE type rather than a scalar} } */
  svclasta (pg, 1, pg); /* { dg-error {'svclasta' has no form that takes 'svbool_t' arguments} } */
  svclasta (pg, i, s32);
  svclasta (pg, s32, 1); /* { dg-error {passing 'int' to argument 3 of 'svclasta', which expects an SVE type rather than a scalar} } */
  svclasta (pg, s32, s64); /* { dg-error {passing 'svint64_t' to argument 3 of 'svclasta', but argument 2 had type 'svint32_t'} } */
  svclasta (pg, pg, pg); /* { dg-error {'svclasta' has no form that takes 'svbool_t' arguments} } */
}
