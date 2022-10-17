/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=thunk-extern -mharden-sls=all" } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void foo (void);

void
bar (void)
{
  foo ();
}

/* { dg-final { scan-assembler "jmp\[ \t\]+_?foo" } } */
/* { dg-final { scan-assembler-not {int3} } } */
