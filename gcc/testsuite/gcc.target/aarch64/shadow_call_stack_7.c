/* Verify:
     * -fomit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18.
     * without outgoing.
     * total frame size <= 256.
     * callee-saved reg: x19, x30.
     * optimized code should use "stp   x19, x30, [sp, -x]!" to save x19, x30 in prologue.
     * optimized code should use "ldr   x19, [sp], x" to restore x19 only.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18 --save-temps" } */

#include "test_frame_common.h"

t_frame_pattern (func1, 200, "x19")

/* { dg-final { scan-assembler-times {stp\tx19, x30, \[sp, -[0-9]+\]!} 1 } } */
/* { dg-final { scan-assembler {ldr\tx19, \[sp\], [0-9]+} } } */

