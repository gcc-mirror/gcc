/* PR c/77754 */

int fn3();

void (**fn5) (int[][fn3 ()]);

void fn1 () {
  int a[10][fn3 ()];
  (**fn5) (a);
}
