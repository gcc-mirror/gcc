/* PR c/14492 */
/* { dg-options "" } */

int main() {
  double d = 1.0;
  char x[(int) d];
  return 0;
}
