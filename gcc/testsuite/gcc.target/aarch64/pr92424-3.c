/* { dg-do "compile" } */
/* { dg-options "-O1" } */

/* Test the placement of the .LPFE0 label.  */

void
__attribute__ ((target("branch-protection=bti+pac-ret+leaf"),
		patchable_function_entry (1, 0)))
f10_pac ()
{
}
/* { dg-final { scan-assembler "hint\t34 // bti c\n.*\.LPFE0:\n\tnop\n.*\thint\t25 // paciasp\n.*\thint\t29 // autiasp\n.*\tret\n" } } */
