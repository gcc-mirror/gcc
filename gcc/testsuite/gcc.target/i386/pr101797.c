/* PR target/101797 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int a;
int main() {
  int b, c, d, e = 0;
  if (a) {
    c += a;
    e = ~(a % c);
    e || c || (b & d);
  }
  a = e;
  return 0;
}
