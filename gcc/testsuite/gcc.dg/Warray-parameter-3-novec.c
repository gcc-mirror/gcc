/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   { dg-do compile }
   { dg-options "-Wall -Warray-parameter=1" } */

/* Also verify that -Warray-bounds doesn't trigger for ordinary array
   parameters...  */
#pragma GCC optimize ("2,no-tree-vectorize")

/* ...but does for static arrays.  */
__attribute__ ((noipa)) void
gcas3 (char a[static 3])
{
  a[0] = 0; a[1] = 1; a[2] = 2;
  a[3] = 3;                   // { dg-warning "\\\[-Warray-bounds" }
}
