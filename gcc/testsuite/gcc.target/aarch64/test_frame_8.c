/* Verify:
     * -fomit-frame-pointer.
     * with outgoing.
     * total frame size bigger than 512.
     * number of callee-saved reg == 1.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test8, 700, , 8, a[8])
t_frame_run (test8)

/* { dg-final { scan-assembler-times "str\tx30, \\\[sp, -\[0-9\]+\\\]!" 3 } } */
/* { dg-final { scan-assembler-times "ldr\tx30, \\\[sp\\\], \[0-9\]+" 3 } } */

/* { dg-final { cleanup-saved-temps } } */
