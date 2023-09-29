/* { dg-do compile } */

int a, b, c, e, g;
short *d;
unsigned char f;
int h() {
  f &= g;
  for (; b; b++) {
    a = 2;
    for (; a; a--)
      c = 0;
    if (c)
      continue;
    e = (unsigned short)*d >> f;
  }
}
