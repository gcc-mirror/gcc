/* { dg-do compile } */

int a, *b = &a, c, d;
int main() {
  *b;
  if (a) {
  L1:
    a = 0;
  L2:
    if (d) {
      while (b)
        ;
      goto L1;
    }
  }
  if (c)
    goto L2;
  return 0;
}
