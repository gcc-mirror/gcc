/* { dg-do run } */
/* { dg-options "-O" } */

/* PR tree-optimization/118514 */

/* Check that we don't pull optimized references that could trap out of
   loops.  */

int a, c = 1;
unsigned char b[1], d;
int main() {
  while (a || !c) {
    signed char e = b[1000000000];
    d = e < 0 || b[1000000000] > 1;
    if (d)
      __builtin_abort ();
  }
  return 0;
}
