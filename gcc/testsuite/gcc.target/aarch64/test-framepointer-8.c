/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer -momit-leaf-frame-pointer -fno-inline --save-temps" } */

#include "asm-adder-clobber-lr.c"

/* omit-frame-pointer is FALSE.
   omit-leaf-frame-pointer is TRUE.
   LR is being clobbered in the leaf.

   Unless we are removing all frame records (which we aren't), it's
   not OK to remove the frame record for a leaf where LR is clobbered.
   Therefore, we expect a frame record in main and leaf.  */

/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */

