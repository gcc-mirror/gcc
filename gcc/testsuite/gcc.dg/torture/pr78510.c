/* PR middle-end/78510 */
/* { dg-do compile } */
int a, b, c, e, f;
char d;
short g, h;
char fn1(int p1) {
  for (;;) {
    h = p1 << 2;
    int i = h;
    g = i > 32767 >> 13 ? i : i << 3;
    f = a ?: c;
    if (e)
      return d;
  }
}

static int fn2() { fn1(0 || b); }

int main() { fn2(); return 0; }
