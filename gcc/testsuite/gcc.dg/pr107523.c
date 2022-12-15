/* { dg-do run } */
/* { dg-options "-O2 " } */

int a, b = 1;
unsigned int c = 1;
int main() {
  int d = 1, f;
  if (b)
    d = 0;
  a = -1;
  b = ~d ^ 465984011;
 L1:;
  if (b < 2)
    f = b;
  b = f;
  if (f <= a) {
    int g = -(a && 1), h = g - f && a, i = ~(c / f) && 1 % (a | h);
    if (c) {
      g = f;
      if (i || (g && (g > -465984012)))
        goto L2;
    }
    c = g | f / c;
  }
  if (0)
  L2:
    a = 0;
  if (a <= c)
    goto L1;
  return 0;
}

