/* PR c/77754 */

int fn3();

void fn4(int[][fn3 ()]);
void fn4(int x[][fn3 ()])
{
}

void fn1() {
  void fn2(int[][fn3 ()]);
  int a[10][fn3 ()];
  fn4 (a);
}
