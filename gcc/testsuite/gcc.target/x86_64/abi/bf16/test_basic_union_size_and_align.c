/* Test of simple unions, size and alignment.  */

#include "bf16-check.h"
#include "defines.h"
#include "macros.h"

static void
do_test (void)
{
  /* Floating point types.  */
  check_basic_union_size_and_align(__bf16, TYPE_SIZE_BF16, TYPE_ALIGN_BF16);
}
