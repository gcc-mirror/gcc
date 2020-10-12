/* { dg-do "compile" { target *-*-linux* } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-O1 -fcf-protection -mmanual-endbr -mfentry -pg -fasynchronous-unwind-tables" } */

/* Test the placement of the .LPFE1 label.  */

void
__attribute__ ((cf_check,patchable_function_entry (1, 0)))
f10_endbr (void)
{
}

/* { dg-final { scan-assembler "\t\.cfi_startproc\n\tendbr(32|64)\n.*\.LPFE1:\n\tnop\n1:\tcall\t__fentry__\n\tret\n" } } */
