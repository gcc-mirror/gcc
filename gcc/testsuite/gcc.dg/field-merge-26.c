/* { dg-do run } */
/* { dg-options "-O1 -fno-tree-ccp -fno-tree-copy-prop -fno-tree-forwprop -fno-tree-fre" } */

/* PR tree-optimization/118805 */

/* Test that ifcombine doesn't get confused by tests for the sign bit of
   extended values that would normally be folded before.  */

unsigned char a = 255;
int b;
int main() {
  int c = 0;
  if (c > a && a >= 255)
    __builtin_abort ();
  if (c <= a && a == 254)
    __builtin_abort ();
  if (a < c || a != 255)
    __builtin_abort ();
  return 0;
}
