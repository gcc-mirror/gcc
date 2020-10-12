/* { dg-do compile } */
/* { dg-options "-std=gnu99 --patchable-function-entry=2,1" } */

/* The purpose of this test is to make sure the right instruction is
   generated for NOPs.  See bpf.md for a description on why this is
   important.  */

int
foo ()
{
  return 0;
}

/* { dg-final { scan-assembler "foo:\n\t*ja\t0" } } */
