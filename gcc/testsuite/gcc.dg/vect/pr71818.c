/* { dg-do compile } */

char a;
short b;
int c, d;
void fn1() {
  char e = 75, g;
  unsigned char *f = &e;
  a = 21;
  for (; a <= 48; a++) {
    for (; e <= 6;)
      ;
    g -= e -= b || g <= c;
  }
  d = *f;
}
