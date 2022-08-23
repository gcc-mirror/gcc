/* This checks size and alignment of structs with a single basic type
   element. All basic types are checked.  */

#include "bf16-check.h"
#include "defines.h"
#include "macros.h"


static void
do_test (void)
{
  /* Floating point types.  */
  check_basic_struct_size_and_align(__bf16, TYPE_SIZE_BF16, TYPE_ALIGN_BF16);
}
