/* PR rtl-optimization/127795 */

/* Verify that delay-slot reorganization deletes the dead add with and
   without debug information.  -fno-tree-ter keeps the add separate, while
   -fno-var-tracking keeps the debug marker until the dbr pass.  Falling off
   the end of the function is required to expose the deletion path.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ter -fno-var-tracking -fcompare-debug" } */

int
foo (int a, int b)
{
  int e = a + b;
  if (e >= 97)
    return a;
}

/* { dg-final { scan-assembler-not "add\t%" } } */
