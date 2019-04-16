/* PR c/77754 */

int fn3();

typedef void (*fn6) (int[][fn3 ()]);
fn6 **fn7;

void fn1 () {
  int a[10][fn3 ()];
  (**fn7) (a);
}
