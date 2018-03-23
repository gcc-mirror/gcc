/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct {
  long a;
  long b;
} c;

c *d;
char e, f, g;
void h() {
  d[0].a = &g - &f;
  d[0].b = &e - &f;
}
