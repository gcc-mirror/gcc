/* { dg-do compile } */
/* { dg-options "-O2 -mindirect-branch=thunk -mharden-sls=all" } */
/* { dg-additional-options "-fno-pic" { target { ! *-*-darwin* } } } */

extern void (*fptr) (void);

void
foo (void)
{
  fptr ();
}

/* { dg-final { scan-assembler "jmp\[ \t\]+_?__x86_indirect_thunk_(r|e)ax" } } */
/* { dg-final { scan-assembler-times "int3" 2 } } */
