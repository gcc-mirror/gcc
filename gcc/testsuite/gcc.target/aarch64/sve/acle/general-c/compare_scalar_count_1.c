/* { dg-do compile } */

#include <arm_sve.h>
#include <stdbool.h>

#pragma GCC target "+sve2+sme2"

enum signed_enum { SA = -1, SB };
enum unsigned_enum { UA, UB };

void
test (int32_t s32, int64_t s64, uint16_t u16, uint32_t u32, uint64_t u64,
      bool b, int *ptr, float f32, svbool_t pg, svint32_t vec)
  __arm_streaming
{
  svwhilele_c8 (s64, 2); /* { dg-error {too few arguments to function 'svwhilele_c8'} } */
  svwhilele_c8 (s64, s64, 2, 2); /* { dg-error {too many arguments to function 'svwhilele_c8'} } */

  svwhilele_c8 (b, b, 2); /* { dg-error {passing '_Bool' and '_Bool' to arguments 1 and 2 of 'svwhilele_c8', which expects a pair of 64-bit integers} } */
  svwhilele_c8 (u16, u16, 2); /* { dg-error {expects a pair of 64-bit integers} } */
  svwhilele_c8 (ptr, ptr, 2); /* { dg-error {expects a pair of 64-bit integers} } */
  svwhilele_c8 (f32, f32, 2); /* { dg-error {expects a pair of 64-bit integers} } */
  svwhilele_c8 (pg, pg, 2); /* { dg-error {expects a pair of 64-bit integers} } */
  svwhilele_c8 (vec, vec, 2); /* { dg-error {expects a pair of 64-bit integers} } */
  svwhilele_c8 (0, 0, 2); /* { dg-error {expects a pair of 64-bit integers} } */
  svwhilele_c8 (s32, s32, 2); /* { dg-error {expects a pair of 64-bit integers} } */

  svwhilele_c8 (0, s64, 2);
  svwhilele_c8 (0U, s64, 2);
  svwhilele_c8 (0, u64, 2); /* { dg-error {mismatched integer types} } */
  svwhilele_c8 (0U, u64, 2);

  svwhilele_c8 (s32, s64, 2);
  svwhilele_c8 (u32, s64, 2);
  svwhilele_c8 (s32, u64, 2); /* { dg-error {mismatched integer types} } */
  svwhilele_c8 (u32, u64, 2);

  svwhilele_c8 (s64, s64, 2);
  svwhilele_c8 (u64, s64, 2); /* { dg-error {mismatched integer types} } */
  svwhilele_c8 (s64, u64, 2); /* { dg-error {mismatched integer types} } */
  svwhilele_c8 (u64, u64, 2);

  svwhilele_c8 (s64, 0, 2);
  svwhilele_c8 (s64, 0U, 2);
  svwhilele_c8 (u64, 0, 2); /* { dg-error {mismatched integer types} } */
  svwhilele_c8 (u64, 0U, 2);

  svwhilele_c8 (s64, s32, 2);
  svwhilele_c8 (s64, u32, 2);
  svwhilele_c8 (u64, s32, 2); /* { dg-error {mismatched integer types} } */
  svwhilele_c8 (u64, u32, 2);

  svwhilele_c8 (u64, u64, u64); /* { dg-error {argument 3 of 'svwhilele_c8' must be an integer constant expression} } */
  svwhilele_c8 (u64, u64, 1); /* { dg-error {passing 1 to argument 3 of 'svwhilele_c8', which expects either 2 or 4} } */
}
