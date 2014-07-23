/* Verify:
     * -fomit-frame-pointer.
     * with outgoing.
     * total frame size > 512.
       area except outgoing <= 512
     * number of callee-saved reg = 1.
     * Split stack adjustment into two subtractions.
       the first subtractions couldn't be optimized
       into "str !" as it's > 256.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test9, 480, , 24, a[8], a[9], a[10])
t_frame_run (test9)
