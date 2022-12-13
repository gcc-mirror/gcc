/* { dg-do "compile" { target *-*-linux* } } */
/* { dg-options "-O1 -fpatchable-function-entry=1 -mfentry -pg -fasynchronous-unwind-tables" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

/* Test the placement of the .LPFE0 label.  */

void
foo (void)
{
}

/* { dg-final { scan-assembler "\t\.cfi_startproc\n.*\.LPFE0:\n\tnop\n1:\tcall\t\[^\n\]*__fentry__\[^\n\]*\n\tret\n" } } */
