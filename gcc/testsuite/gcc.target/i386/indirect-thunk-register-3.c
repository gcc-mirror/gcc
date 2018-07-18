/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=thunk-extern -mindirect-branch-register -fno-pic" } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch;

void
male_indirect_jump (long offset)
{
  dispatch(offset);
}

/* { dg-final { scan-assembler "jmp\[ \t\]*__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler-not "push(?:l|q)\[ \t\]*_?dispatch"  } } */
/* { dg-final { scan-assembler-not "pushq\[ \t\]%rax" } } */
/* { dg-final { scan-assembler-not {\t(pause|pause|nop)} } } */
/* { dg-final { scan-assembler-not "jmp\[ \t\]*\.LIND" } } */
/* { dg-final { scan-assembler-not "call\[ \t\]*\.LIND" } } */
