/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld" } */

extern void bar (int);

void f1 (void){  bar (1); }
__attribute__((interrupt))
void f2 (void *frame){  bar (2); }
void f3 (void){  bar (3); }
__attribute__((interrupt))
void f4 (void *frame){  bar (4); }
void f5 (void){  bar (5); }

/* { dg-final { scan-assembler-times "push.\t%.ax" 2 } } */
/* { dg-final { scan-assembler-times "pop.\t%.ax" 2 } } */
/* { dg-final { scan-assembler-times "iret" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 2 } } */
