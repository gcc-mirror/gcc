/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=thunk -mindirect-branch-register " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

typedef void (*dispatch_t)(long offset);

dispatch_t dispatch;

void
male_indirect_jump (long offset)
{
  dispatch(offset);
}

/* { dg-final { scan-assembler "jmp\[ \t\]*_?__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler {call[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler "mov\[ \t\](%eax|%rax), \\((%esp|%rsp)\\)" } } */
/* { dg-final { scan-assembler {\tpause} } } */
/* { dg-final { scan-assembler-not "push(?:l|q)\[ \t\]*_?dispatch"  } } */
/* { dg-final { scan-assembler-not "pushq\[ \t\]%rax" } } */
/* { dg-final { scan-assembler-not "_?__x86_indirect_thunk\n" } } */
