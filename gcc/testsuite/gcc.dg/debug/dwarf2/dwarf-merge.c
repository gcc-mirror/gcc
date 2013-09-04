/* Verify that mergeable strings are used in the CU DIE.  */
/* { dg-do compile } */
/* { dg-require-effective-target string_merging } */
/* { dg-options "-O2 -gdwarf -dA" } */
/* { dg-final { scan-assembler "DW_AT_producer: \"GNU C" } } */
/* { dg-final { scan-assembler-not "GNU C\[^\\n\\r\]*DW_AT_producer" } } */

void func (void)
{
}
