/* PR target/128388 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapxf -mindirect-branch-cs-prefix -mindirect-branch=thunk-extern -ffixed-rax -ffixed-rbx -ffixed-rcx -ffixed-rdx -ffixed-rsi -ffixed-rdi -ffixed-rbp -ffixed-r8 -ffixed-r9 -ffixed-r10 -ffixed-r11 -ffixed-r12 -ffixed-r13 -ffixed-r14 -ffixed-r15" } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*fptr) (void);

void
foo (void)
{
  fptr ();
}

int
bar (void)
{
  fptr ();
  return 0;
}

/* { dg-final { scan-assembler-times "\tcs\n\tjmp\[ \t\]+_?__x86_indirect_thunk_r16" 1 } } */
/* { dg-final { scan-assembler-times "\tcs\n\tcall\[ \t\]+_?__x86_indirect_thunk_r16" 1 } } */
