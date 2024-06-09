/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

int *a, *f;
char b, c;
int ***d;
static int ****e = &d;
void g() {
  c = 3;
  for (; c; c--)
    if (c < 8) {
      f = 0;
      ***e = a;
    }
  if (b)
    ***d = 0;
}
