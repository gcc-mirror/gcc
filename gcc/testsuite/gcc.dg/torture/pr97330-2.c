/* { dg-do compile } */

int a, b, d;
char c, e;
void f(void) {
  char g = c;
  if (b)
    goto h;
  while (d) {
    e = c;
  h:
    d -= a;
  }
}
