/* This checks sizes of basic types.  */

#include "defines.h"
#include "macros.h"


int
main (void)
{
  /* Floating point types.  */
  check_size(__bf16, TYPE_SIZE_BF16);

  return 0;
}
