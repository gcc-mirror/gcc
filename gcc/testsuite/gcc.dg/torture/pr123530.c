/* { dg-do compile } */
int a, b, c;
void d() {
  int e = -1;
  (c >> e | ~(b << 1) ^ 1) & a;
}
