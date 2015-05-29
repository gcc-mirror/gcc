/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -fno-inline --save-temps" } */

#include "asm-adder-no-clobber-lr.c"

/* omit-frame-pointer is FALSE.
   omit-leaf-frame-pointer is FALSE.
   LR is not being clobbered in the leaf.

   With no frame pointer omissions, we expect a frame record
   for main and the leaf.  */

/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */

