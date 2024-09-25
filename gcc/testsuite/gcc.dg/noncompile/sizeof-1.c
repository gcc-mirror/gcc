/* PR c/115642 */
/* { dg-do compile } */

void f (int N) {
  int a[2][N];
  sizeof ((int [2][N])a); /* { dg-error "cast specifies array type" } */
}
