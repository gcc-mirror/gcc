#include <arm_sve.h>

void
test (int32_t sw, enum svpattern pat, int i)
{
  svqincb_pat_n_s32 (sw, pat, 1); /* { dg-error {argument 2 of 'svqincb_pat_n_s32' must be an integer constant expression} } */
  svqincb_pat_n_s32 (sw, i, 1); /* { dg-error {argument 2 of 'svqincb_pat_n_s32' must be an integer constant expression} } */
  svqincb_pat_n_s32 (sw, (enum svpattern) -1, 1); /* { dg-error {passing 4294967295 to argument 2 of 'svqincb_pat_n_s32', which expects a valid 'enum svpattern' value} } */
  svqincb_pat_n_s32 (sw, (enum svpattern) 0, 1);
  svqincb_pat_n_s32 (sw, (enum svpattern) 13, 1);
  svqincb_pat_n_s32 (sw, (enum svpattern) 14, 1); /* { dg-error {passing 14 to argument 2 of 'svqincb_pat_n_s32', which expects a valid 'enum svpattern' value} } */
  svqincb_pat_n_s32 (sw, (enum svpattern) 28, 1); /* { dg-error {passing 28 to argument 2 of 'svqincb_pat_n_s32', which expects a valid 'enum svpattern' value} } */
  svqincb_pat_n_s32 (sw, (enum svpattern) 29, 1);
  svqincb_pat_n_s32 (sw, (enum svpattern) 31, 1);
  svqincb_pat_n_s32 (sw, (enum svpattern) 32, 1); /* { dg-error {passing 32 to argument 2 of 'svqincb_pat_n_s32', which expects a valid 'enum svpattern' value} } */

  svqincb_pat_n_s32 (sw, SV_POW2, -1); /* { dg-error {passing -1 to argument 3 of 'svqincb_pat_n_s32', which expects a value in the range \[1, 16\]} } */
  svqincb_pat_n_s32 (sw, SV_POW2, 0); /* { dg-error {passing 0 to argument 3 of 'svqincb_pat_n_s32', which expects a value in the range \[1, 16\]} } */
  svqincb_pat_n_s32 (sw, SV_POW2, 1);
  svqincb_pat_n_s32 (sw, SV_POW2, 2);
  svqincb_pat_n_s32 (sw, SV_POW2, 16);
  svqincb_pat_n_s32 (sw, SV_POW2, 17); /* { dg-error {passing 17 to argument 3 of 'svqincb_pat_n_s32', which expects a value in the range \[1, 16\]} } */
}
