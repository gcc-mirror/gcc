/* { dg-do "compile" } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-O1 -fpatchable-function-entry=1 -mfentry -pg -fasynchronous-unwind-tables" } */

/* Test the placement of the .LPFE1 label.  */

void
foo (void)
{
}

/* { dg-final { scan-assembler "\t\.cfi_startproc\n.*\.LPFE1:\n\tnop\n1:\tcall\t__fentry__\n\tret\n" } } */
