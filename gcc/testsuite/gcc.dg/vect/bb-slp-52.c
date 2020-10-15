/* { dg-do compile } */

int a, e;
void g(float);
typedef struct {
  float b, c;
} d;
d f;
void h(double i, double j) {
  if (a && e)
    return;
  f.b = j;
  f.c = i;
  g(i);
}
