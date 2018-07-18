/* PR target/82361 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mtune=generic -masm=att -m8bit-idiv" } */
/* We should be able to optimize all %eax to %rax zero extensions, because
   div and idiv instructions with 32-bit operands zero-extend both results.   */
/* { dg-final { scan-assembler-not "movl\t%eax, %eax" } } */
/* Ditto %edx to %rdx zero extensions.  */
/* { dg-final { scan-assembler-not "movl\t%edx, %edx" } } */

#include "pr82361-1.c"
