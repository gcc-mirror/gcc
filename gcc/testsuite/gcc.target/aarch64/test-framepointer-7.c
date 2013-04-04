/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer -momit-leaf-frame-pointer -fno-inline --save-temps" } */

#include "asm-adder-clobber-lr.c"

/* omit-frame-pointer is TRUE.
   omit-leaf-frame-pointer is true, but irrelevant due to omit-frame-pointer.
   LR is being clobbered in the leaf.

   Since we asked to have no frame pointers anywhere, we expect no frame
   record in main or the leaf.  */

/* { dg-final { scan-assembler-not "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" } } */

/* { dg-final { cleanup-saved-temps } } */
