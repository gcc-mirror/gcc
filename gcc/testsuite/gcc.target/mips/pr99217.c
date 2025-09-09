/* { dg-do compile } */
/* { dg-additional-options "-fpatchable-function-entry=1" } */
/* { dg-final { scan-assembler "foo:\n.*.LPFE0:\n\t.set\tnoreorder\n\tnop\n\t.set\treorder" } } */

/* Test the placement of the .LPFE0 label.  */

void
foo (void)
{
}
