/* Verify:
     * with outgoing.
     * total frame size > 512.
       area except outgoing <= 512
     * number of callee-save reg >= 2.
     * split the stack adjustment into two substractions,
       the first could be optimized into "stp !".  */

/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test15, 480, , 8, a[8])
t_frame_run (test15)

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 3 } } */
/* { dg-final { cleanup-saved-temps } } */
