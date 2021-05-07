/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */

int *a;
static int b, c, d, e, g, h;
int f;
void i() {
  int *j[] = {&e, &b, &b, &d, &b, &b, &g, &e, &g, &b, &b,
              &b, &b, &g, &e, &e, &b, &b, &d, &b, &b, &e,
              &e, &g, &b, &b, &b, &b, &g, &e, &g, &c, &e};
  int **k = &j[5];
  for (; f;)
    b |= *a;
  *k = &h;
}
int main() {}
