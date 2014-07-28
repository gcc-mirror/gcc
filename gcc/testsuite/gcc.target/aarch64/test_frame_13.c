/* Verify:
     * without outgoing.
     * total frame size > 512.
     * number of callee-save reg >= 2.
     * split the stack adjustment into two substractions,
       the second could be optimized into "stp !".  */

/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (test13, 700, )
t_frame_run (test13)

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */
/* { dg-final { cleanup-saved-temps } } */
