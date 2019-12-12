/* Verify that used enums are output.  */
/* { dg-do compile } */
/* { dg-additional-options "-fno-eliminate-unused-debug-symbols" { target powerpc-ibm-aix* } } */
/* { dg-final { scan-assembler "JTI_MAX" } } */

int var;

enum java_tree_index
{
  JTI_MAX
};

void function (void)
{
  var = JTI_MAX;
}
 
