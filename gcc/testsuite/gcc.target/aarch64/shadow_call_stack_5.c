/* Verify:
     * -fno-omit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18.
     * without outgoing.
     * total frame size <= 512 but > 256.
     * callee-saved reg: x29, x30.
     * optimized code should use "stp	x29, x30, [sp]" to save frame chain.
     * optimized code should use "ldr	x29, [sp]" to restore x29 only.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18 --save-temps -fno-stack-protector" } */

#include "test_frame_common.h"

t_frame_pattern (func1, 400, )

/* { dg-final { scan-assembler-times {stp\tx29, x30, \[sp\]} 1 } } */
/* { dg-final { scan-assembler {ldr\tx29, \[sp\]} } } */

