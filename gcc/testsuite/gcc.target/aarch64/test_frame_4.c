/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size <= 512 but > 256.
     * number of callee-save reg >= 2.
     * we can use "stp !" to optimize stack adjustment.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (test4, 400, "x19")
t_frame_run (test4)

/* { dg-final { scan-assembler-times "stp\tx19, x30, \\\[sp, -\[0-9\]+\\\]!" 1 } } */
/* { dg-final { scan-assembler "ldp\tx19, x30, \\\[sp\\\], \[0-9\]+" } } */

