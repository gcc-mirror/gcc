/* { dg-do compile { target { { ! ia32 } && cfi } } } */
/* { dg-options "-O2 -mapx-features=push2pop2 -fomit-frame-pointer" } */

extern int bar (int);

void foo ()
{
  int a,b,c,d,e,f,i;
  a = bar (5);
  b = bar (a);
  c = bar (b);
  d = bar (c);
  e = bar (d);
  f = bar (e);
  for (i = 1; i < 10; i++)
  {
    a += bar (a + i) + bar (b + i) +
         bar (c + i) + bar (d + i) +
         bar (e + i) + bar (f + i);
  }
}

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
