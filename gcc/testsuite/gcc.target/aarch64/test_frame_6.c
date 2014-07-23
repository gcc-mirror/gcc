/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size > 512.
     * number of callee-saved reg == 1.
     * split stack adjustment into two subtractions.
       the second subtraction should use "str !".  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

#include "test_frame_common.h"

t_frame_pattern (test6, 700, )
t_frame_run (test6)
