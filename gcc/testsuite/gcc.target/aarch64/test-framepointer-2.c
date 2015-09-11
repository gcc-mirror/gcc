/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer -mno-omit-leaf-frame-pointer -fno-inline --save-temps" } */

#include "asm-adder-no-clobber-lr.c"

/* omit-frame-pointer is TRUE.
   omit-leaf-frame-pointer is false, but irrelevant due to omit-frame-pointer.
   LR is not being clobbered in the leaf.

   Since we asked to have no frame pointers anywhere, we expect no frame
   record in main or the leaf.  */

/* { dg-final { scan-assembler-not "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" } } */

