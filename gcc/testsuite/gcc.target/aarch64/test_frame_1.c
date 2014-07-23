/* Verify:
     * -fomit-frame-pointer.
     * withoug outgoing.
     * total frame size <= 256.
     * number of callee-save reg == 1.
     * optimized code should use "str !" for stack adjustment.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

#include "test_frame_common.h"

t_frame_pattern (test1, 200, )
t_frame_run (test1)
