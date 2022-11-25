/* { dg-do "compile" { target *-*-linux* } } */
/* { dg-options "-O1 -fcf-protection -mmanual-endbr -fasynchronous-unwind-tables" } */

/* Test the placement of the .LPFE0 label.  */

void
__attribute__ ((cf_check,patchable_function_entry (1, 0)))
f10_endbr (void)
{
}

/* { dg-final { scan-assembler "\t\.cfi_startproc\n\tendbr(32|64)\n.*\.LPFE0:\n\tnop\n\tret\n" } } */
