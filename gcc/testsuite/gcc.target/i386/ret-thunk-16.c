/* { dg-do compile } */
/* { dg-options "-O2 -mfunction-return=thunk-inline -mindirect-branch=thunk-extern -fno-pic" } */

extern void (*bar) (void);

__attribute__ ((function_return("keep"), indirect_branch("keep")))
int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler-not "__x86_indirect_thunk" } } */
/* { dg-final { scan-assembler-not "__x86_return_thunk" } } */
/* { dg-final { scan-assembler-not {\t(lfence|pause)} } } */
/* { dg-final { scan-assembler-not "jmp\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler-not "call\[ \t\]*\.LIND" } } */
