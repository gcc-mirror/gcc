/* PR c/108880 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

long a;
short b, e;
char c;
int d, f, g;
void h() {
  int i;
  f &= i ^= (((g &= 0 / d / d % 8 << 0 << 2) % a >> e) / c >> b) / 1 % 8 << 3;
}
int main() {}
