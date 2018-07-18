/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size > 512.
     * number of callee-saved reg == 2.
     * use a single stack adjustment, no writeback.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (test7, 700, "x19")
t_frame_run (test7)

/* { dg-final { scan-assembler-times "stp\tx19, x30, \\\[sp]" 1 } } */
/* { dg-final { scan-assembler "ldp\tx19, x30, \\\[sp\\\]" } } */

