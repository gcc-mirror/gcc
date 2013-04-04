/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer -momit-leaf-frame-pointer -fno-inline --save-temps" } */

#include "asm-adder-no-clobber-lr.c"

/* omit-frame-pointer is FALSE.
   omit-leaf-frame-pointer is TRUE.
   LR is not being clobbered in the leaf.

   Unless we are removing all frame records, it's OK to remove the frame
   record for a leaf where LR is not clobbered.  Therefore, we expect a
   frame record only in main.  */

/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 1 } } */

/* { dg-final { cleanup-saved-temps } } */
