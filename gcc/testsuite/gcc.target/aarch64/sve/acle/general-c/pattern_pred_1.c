#include <arm_sve.h>

void
test ()
{
  svptrue_pat_b16 ((enum svpattern) -1); /* { dg-error {passing 4294967295 to argument 1 of 'svptrue_pat_b16', which expects a valid 'enum svpattern' value} } */
  svptrue_pat_b16 ((enum svpattern) 0);
  svptrue_pat_b16 ((enum svpattern) 13);
  svptrue_pat_b16 ((enum svpattern) 14); /* { dg-error {passing 14 to argument 1 of 'svptrue_pat_b16', which expects a valid 'enum svpattern' value} } */
  svptrue_pat_b16 ((enum svpattern) 28); /* { dg-error {passing 28 to argument 1 of 'svptrue_pat_b16', which expects a valid 'enum svpattern' value} } */
  svptrue_pat_b16 ((enum svpattern) 29);
  svptrue_pat_b16 ((enum svpattern) 31);
  svptrue_pat_b16 ((enum svpattern) 32); /* { dg-error {passing 32 to argument 1 of 'svptrue_pat_b16', which expects a valid 'enum svpattern' value} } */
}
