/* { dg-do compile } */
/* { dg-additional-options "-fpatchable-function-entry=2,1" } */
/* { dg-final { scan-assembler "foo:\n\t.set\tnoreorder\n\tnop\n\t.set\treorder" } } */
/* { dg-final { scan-assembler ".*.LPFE0:\n\t.set\tnoreorder\n\tnop\n\t.set\treorder\n\t.ent\tfoo\n" } } */

/* Test the placement of the .LPFE0 label.  */

void
foo (void)
{
}
