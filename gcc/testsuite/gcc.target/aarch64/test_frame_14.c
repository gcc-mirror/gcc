/* Verify:
     * with outgoing.
     * total frame size > 512.
     * number of callee-save reg >= 2.  */

/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test14, 700, , 8, a[8])
t_frame_run (test14)

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, \[0-9\]+\\\]" 1 } } */
