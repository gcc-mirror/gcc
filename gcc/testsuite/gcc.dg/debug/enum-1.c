/* Verify that used enums are output.  */
/* { dg-do compile } */
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
 
