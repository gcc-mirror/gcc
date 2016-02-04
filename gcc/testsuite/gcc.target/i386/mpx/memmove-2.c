/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include <stdint.h>
#include <string.h>
#include "mpx-check.h"

#ifdef __i386__
/* i386 directory size is 4MB.  */
#define MPX_NUM_L2_BITS 10
#define MPX_NUM_IGN_BITS 2
#else /* __i386__ */
/* x86_64 directory size is 2GB.  */
#define MPX_NUM_L2_BITS 17
#define MPX_NUM_IGN_BITS 3
#endif /* !__i386__ */


/* bt_num_of_elems is the number of elements in bounds table.  */
unsigned long bt_num_of_elems = (1UL << MPX_NUM_L2_BITS);

/* Function to test MPX wrapper of memmove function.
   Check case with no BT allocated for data.  */

int
mpx_test (int argc, const char **argv)
{
  void **arr = 0;
  posix_memalign ((void **) (&arr),
           1UL << (MPX_NUM_L2_BITS + MPX_NUM_IGN_BITS),
           2 * bt_num_of_elems * sizeof (void *));
  void **src = arr, **dst = arr, **ptr = arr;
  src += 10;
  dst += 1;
  ptr += bt_num_of_elems + 100;
  ptr[0] = __bnd_set_ptr_bounds (arr + 1, sizeof (void *) + 1);
  memmove (dst, src, 5 * sizeof (void *));
  return 0;
}
