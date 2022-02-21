/* Verify:
     * -fomit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18.
     * without outgoing.
     * total frame size <= 256.
     * callee-saved reg: x30 only.
     * optimized code should use "str   x30, [sp]" to save x30 in prologue.
     * optimized code should not restore x30 in epilogue.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18 --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (func1, 200, )

/* { dg-final { scan-assembler-times {str\tx30, \[sp\]} 1 } } */
/* { dg-final { scan-assembler-not {ld[r|p]\tx30, \[sp} } } */

