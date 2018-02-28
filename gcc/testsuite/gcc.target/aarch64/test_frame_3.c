/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size <= 512 but > 256.
     * number of callee-save reg == 1.
     * we can't use "str !" to optimize stack adjustment.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

#include "test_frame_common.h"

t_frame_pattern (test3, 400, )
t_frame_run (test3)
