/* Verify:
     * -fomit-frame-pointer.
     * with outgoing.
     * total frame size <= 512.
     * one subtraction of the whole frame size.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test5, 300, "x19", 8, a[8])
t_frame_run (test5)

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler "stp\tx\[0-9\]+, x30, \\\[sp, \[0-9\]+\\\]" } } */
