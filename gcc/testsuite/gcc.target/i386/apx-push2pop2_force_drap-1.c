/* { dg-do compile { target { { ! ia32 } && cfi } } } */
/* { dg-options "-O2 -mapx-features=push2pop2 -fomit-frame-pointer -mforce-drap" } */

#include "apx-push2pop2-1.c"


/* { dg-final { scan-assembler-times ".cfi_def_cfa_offset 16" 2 } } */
/* { dg-final { scan-assembler-times "pushq\[^\n\r]*%r15(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_offset 15, -16(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "push2\[\\t \]*\[^\n\r]*%r13\[^\n\r]*%r14\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_def_cfa_offset 32" 2 } } */
/* { dg-final { scan-assembler-times ".cfi_offset 14, -24(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_offset 13, -32(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "push2\[\\t \]*\[^\n\r]*%rbp\[^\n\r]*%r12\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_def_cfa_offset 48" 2 } } */
/* { dg-final { scan-assembler-times ".cfi_offset 12, -40(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_offset 6, -48(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "pushq\[^\n\r]*%rbx(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_def_cfa_offset 56" 2 } } */
/* { dg-final { scan-assembler-times ".cfi_offset 3, -56(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "popq\[^\n\r]*rbx(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "pop2\[\\t \]*\[^\n\r]*%r12\[^\n\r]*%rbp\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_restore 12(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_restore 6(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "pop2\[\\t \]*\[^\n\r]*%r14\[^\n\r]*%r13\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_restore 14(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_restore 13(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "popq\[^\n\r]*%r15(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_def_cfa_offset 8(?:\n|\[ \\t\]+#)" 1 } } */
