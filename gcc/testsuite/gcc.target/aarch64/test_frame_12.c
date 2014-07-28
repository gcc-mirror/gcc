/* Verify:
     * with outgoing.
     * total frame size <= 512.
     * number of callee-save reg >= 2.  */

/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern_outgoing (test12, 400, , 8, a[8])
t_frame_run (test12)

/* { dg-final { scan-assembler-times "sub\tsp, sp, #\[0-9\]+" 1 } } */

/* Check epilogue using write-back.  */
/* { dg-final { scan-assembler-times "ldp\tx29, x30, \\\[sp\\\], \[0-9\]+" 3 } } */

/* { dg-final { cleanup-saved-temps } } */
