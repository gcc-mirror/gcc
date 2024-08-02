// { dg-options "-O2" }

#include <stdlib.h>
#include <arm_sve.h>

#pragma GCC target "+sve2"

typedef unsigned char uchar;

const uchar *
search_line_fast (const uchar *s, const uchar *end)
{
  size_t VL = svcntb();
  svuint8_t arr1, arr2;
  svbool_t pc, pg = svptrue_b8();

  // This should not be loaded inside the loop every time.
  arr2 = svreinterpret_u8(svdup_u32(0x0a0d5c3f));

  for (; s+VL <= end; s += VL) {
    arr1 = svld1_u8(pg, s);
    pc = svmatch_u8(pg, arr1, arr2);

    if (svptest_any(pg, pc)) {
      pc = svbrkb_z(pg, pc);
      return s+svcntp_b8(pg, pc);
    }
  }

  // Handle remainder.
  if (s < end) {
    pg = svwhilelt_b8((size_t)s, (size_t)end);

    arr1 = svld1_u8(pg, s);
    pc = svmatch_u8(pg, arr1, arr2);

    if (svptest_any(pg, pc)) {
      pc = svbrkb_z(pg, pc);
      return s+svcntp_b8(pg, pc);
    }
  }

  return end;
}

// { dg-final { scan-assembler {:\n\tld1b\t[^\n]*\n\tmatch\t[^\n]*\n\tb\.} } }
