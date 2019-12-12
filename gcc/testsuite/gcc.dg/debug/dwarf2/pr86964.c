/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf -feliminate-unused-debug-symbols -dA" } */

struct S { int i; };
extern struct S x;
int y;
int main()
{
  return y;
}

/* We should elide the DIEs for x and S but not y.  */
/* { dg-final { scan-assembler-times "DW_TAG_variable" 2 } } */
/* { dg-final { scan-assembler-not "DW_TAG_structure_type" } } */
