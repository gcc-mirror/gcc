/* Verify:
     * -fomit-frame-pointer.
     * with outgoing.
     * total frame size > 512.
       area except outgoing <= 512
     * number of callee-saved reg = 1.
     * Use a single stack adjustment.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test9, 480, , 24, a[8], a[9], a[10])
t_frame_run (test9)

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler "str\tx30, \\\[sp, \[0-9\]+\\\]" } } */
