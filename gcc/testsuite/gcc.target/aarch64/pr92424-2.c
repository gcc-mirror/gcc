/* { dg-do "compile" } */
/* { dg-options "-O1" } */

/* Test the placement of the .LPFE0 label.  */

void
__attribute__ ((target("branch-protection=bti"),
		patchable_function_entry (1, 0)))
f10_bti ()
{
}
/* { dg-final { scan-assembler "hint\t34 // bti c\n.*\.LPFE0:\n\tnop\n.*\tret\n" } } */
