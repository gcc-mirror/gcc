/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-distribution" } */

extern void abort (void);

signed char a, c;
int b, d, *e = &d, g;
signed static char f;
int main() {
  int h = 0;
  int a_ = a;
  for (; a_ < 1; a = ++a_) {
    int *i[5], **j = &i[4], ***k[3][2] = {{&j}}, ****l = &k[2][1], *****m = &l;
    char *n = &c;
    f = *e = g = 0;
    for (; g < 2; g++) {
      for (b = 0; b < 3; b++)
        h = (h && (*n = 0)) == 0;
      if (g)
        break;
    }
  }
  if (f != 0)
    abort ();
  return 0;
}
