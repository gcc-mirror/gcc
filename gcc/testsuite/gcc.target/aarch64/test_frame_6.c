/* Verify:
     * -fomit-frame-pointer.
     * without outgoing.
     * total frame size > 512.
     * number of callee-saved reg == 1.
     * use a single stack adjustment, no writeback.  */

/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps -fno-stack-protector" } */

#include "test_frame_common.h"

t_frame_pattern (test6, 700, )
t_frame_run (test6)

/* { dg-final { scan-assembler-times "str\tx30, \\\[sp\\\]" 1 } } */
/* { dg-final { scan-assembler "ldr\tx30, \\\[sp\\\]" } } */
/* { dg-final { scan-assembler "ldr\tx30, \\\[sp\\\]," } } */

