/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=keep -mharden-sls=all" } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*fptr) (void);

void
foo (void)
{
  fptr ();
}

/* { dg-final { scan-assembler "jmp\[ \t\]+\\*_?fptr" { target { ! x32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+fptr\\(%rip\\), %eax" { target x32 } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]+\\*%rax" { target x32 } } } */
/* { dg-final { scan-assembler-times "int3" 1 } } */
