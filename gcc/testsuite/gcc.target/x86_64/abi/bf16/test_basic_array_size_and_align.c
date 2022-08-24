/* This checks .  */

#include "defines.h"
#include "macros.h"


int
main (void)
{
  check_array_size_and_align(__bf16, TYPE_SIZE_BF16, TYPE_ALIGN_BF16);

  return 0;
}
