/* { dg-do compile } */
/* { dg-additional-options " -fno-tree-copy-prop -fno-tree-pre -fno-code-hoisting" */

/* PR tree-optimization/121295 */


int a, b, c;
int main() {
  int *d = &a;
  while (b)
    b = (*d &= 10) <= 0 || (*d = c);
  return 0;
}
