/* { dg-do compile } */

int a, b, c, e, h, j;
char d;
short f, g;
static short fn2(int p1) {
  for (;;)
    for (; g; g++)
      if (p1)
        break;
}

static short fn3(void);
static char fn4(char p1) {
  int i;
  for (; d;)
    f = 8;
  for (; f; f = 0)
    for (; i; i++) {
      j = 0;
      for (; j; j++)
        ;
    }
}

static short fn1(short p1) { fn2(b || fn3()); }

short fn3(void) {
  if (c) {
    fn4(e);
    h = 0;
    for (;; h++)
      ;
  }
}

int main() {
  for (; a;)
    fn1(c);
  return 0;
}
