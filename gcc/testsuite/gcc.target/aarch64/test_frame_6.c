/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size > 512.
     * number of callee-saved reg == 1.
     * split stack adjustment into two subtractions.
       the second subtraction should use "str !".  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (test6, 700, )
t_frame_run (test6)

/* { dg-final { scan-assembler-times "str\tx30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */
/* { dg-final { scan-assembler-times "ldr\tx30, \\\[sp\\\], \[0-9\]+" 3 } } */

/* { dg-final { cleanup-saved-temps } } */
