/* This checks alignment of basic types.  */

#include "defines.h"
#include "macros.h"


int
main (void)
{
  /* __bf16 point types.  */
  check_align(__bf16, TYPE_ALIGN_BF16);

  return 0;
}
