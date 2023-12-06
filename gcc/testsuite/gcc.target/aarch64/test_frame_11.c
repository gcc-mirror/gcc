/* Verify:
     * without outgoing.
     * total frame size <= 512.
     * number of callee-save reg >= 2.
     * optimized code should use "stp !" for stack adjustment.  */

/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-stack-protector" } */

#include "test_frame_common.h"

t_frame_pattern (test11, 400, )
t_frame_run (test11)

/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */
