/* { dg-options "-Os" } */

#include <arm_sve.h>
extern char b[];
int x;
void f() {
  while (x) {
    x = svaddv(
        svnot_z(svnot_z(svptrue_pat_b8(SV_VL6),
                        svmov_z(svptrue_pat_b8(SV_VL1),
                                svptrue_pat_b16(SV_VL3))),
                svptrue_pat_b64(SV_VL2)),
        svdup_s32(8193));
    for (int j = x; j; j++)
      b[j] = 0;
  }
}
