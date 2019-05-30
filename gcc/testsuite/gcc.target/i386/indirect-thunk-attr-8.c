/* { dg-do compile } */
/* { dg-options "-O2 -mfunction-return=keep -mindirect-branch=thunk " } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

void func0 (void);
void func1 (void);
void func2 (void);
void func3 (void);
void func4 (void);
void func4 (void);
void func5 (void);

__attribute__ ((indirect_branch("keep")))
void
bar (int i)
{
  switch (i)
    {
    default:
      func0 ();
      break;
    case 1:
      func1 ();
      break;
    case 2:
      func2 ();
      break;
    case 3:
      func3 ();
      break;
    case 4:
      func4 ();
      break;
    case 5:
      func5 ();
      break;
    }
}

/* { dg-final { scan-assembler-not "_?__x86_indirect_thunk" } } */
/* { dg-final { scan-assembler-not {\t(lfence|pause)} } } */
/* { dg-final { scan-assembler-not {jmp[ \t]*\.?LIND} } } */
/* { dg-final { scan-assembler-not {call[ \t]*\.?LIND} } } */
