/* { dg-do run } */
/* { dg-options "-O2" } */

int a, b, c, d;
void k() {
  unsigned f = 1;
  long long g = 4073709551615;
  for (; a; a++)
    for (;;) {
      d = 0;
    L1:
      break;
    }
  if (f)
    for (; a; a++)
      ;
  g || f;
  int i = 0 - f || g;
  long long j = g - f;
  if (j || f) {
    if (g < 4073709551615)
      for (;;)
        ;
    int e = ~f, h = b / ~e;
    if (c)
      goto L2;
    g = f = h;
  }
  g || d;
L2:
  if (c)
    goto L1;
}
int main() { k(); return 0; }
