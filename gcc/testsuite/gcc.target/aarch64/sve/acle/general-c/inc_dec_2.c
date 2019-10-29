#include <arm_sve.h>

void
test (int32_t sw, int i)
{
  svqincb_n_s32 (sw, -1); /* { dg-error {passing -1 to argument 2 of 'svqincb_n_s32', which expects a value in the range \[1, 16\]} } */
  svqincb_n_s32 (sw, 0); /* { dg-error {passing 0 to argument 2 of 'svqincb_n_s32', which expects a value in the range \[1, 16\]} } */
  svqincb_n_s32 (sw, 1);
  svqincb_n_s32 (sw, 2);
  svqincb_n_s32 (sw, 16);
  svqincb_n_s32 (sw, 17); /* { dg-error {passing 17 to argument 2 of 'svqincb_n_s32', which expects a value in the range \[1, 16\]} } */
  svqincb_n_s32 (sw, i); /* { dg-error {argument 2 of 'svqincb_n_s32' must be an integer constant expression} } */
}
