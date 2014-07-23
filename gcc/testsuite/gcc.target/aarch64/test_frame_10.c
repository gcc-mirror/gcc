/* Verify:
     * -fomit-frame-pointer.
     * with outgoing.
     * total frame size > 512.
       area except outgoing <= 512
     * number of callee-saved reg >= 2.
     * Split stack adjustment into two subtractions.
       the first subtractions could be optimized into "stp !".  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test10, 480, "x19", 24, a[8], a[9], a[10])
t_frame_run (test10)
