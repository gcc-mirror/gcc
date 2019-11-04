#include <arm_sve.h>

void
test (svbool_t pg, svint32_t s32, svuint64_t u64, int16_t sh, uint16_t uh,
      int32_t sw, uint32_t uw, int64_t sd, uint64_t ud)
{
  svqincp_b8 (s32); /* { dg-error {too few arguments to function 'svqincp_b8'} } */
  svqincp_b8 (s32, pg, pg); /* { dg-error {too many arguments to function 'svqincp_b8'} } */
  svqincp_b8 (pg, pg); /* { dg-error {passing 'svbool_t' to argument 1 of 'svqincp_b8', which expects a 32-bit or 64-bit integer type} } */
  svqincp_b8 (s32, pg); /* { dg-error {passing 'svint32_t' to argument 1 of 'svqincp_b8', which expects a 32-bit or 64-bit integer type} } */
  svqincp_b8 (sh, pg);
  svqincp_b8 (uh, pg);
  svqincp_b8 (sw, pg);
  svqincp_b8 (uw, pg);
  svqincp_b8 (sd, pg);
  svqincp_b8 (ud, pg);
  svqincp_b8 (ud, 0); /* { dg-error {passing 'int' to argument 2 of 'svqincp_b8', which expects 'svbool_t'} } */
  svqincp_b8 (ud, u64); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svqincp_b8', which expects 'svbool_t'} } */
}
