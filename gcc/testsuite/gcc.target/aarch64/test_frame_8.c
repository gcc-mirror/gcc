/* Verify:
     * -fomit-frame-pointer.
     * with outgoing.
     * total frame size bigger than 512.
     * number of callee-saved reg == 1.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test8, 700, , 8, a[8])
t_frame_run (test8)
