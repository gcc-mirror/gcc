/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size <= 256.
     * number of callee-save regs >= 2.
     * optimized code should use "stp !" for stack adjustment.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (test2, 200, "x19")
t_frame_run (test2)


/* { dg-final { scan-assembler-times "stp\tx19, x30, \\\[sp, -\[0-9\]+\\\]!" 1 } } */
/* { dg-final { scan-assembler "ldp\tx19, x30, \\\[sp\\\], \[0-9\]+" } } */

