/* { dg-additional-options "-fschedule-insns" } */
/* { dg-require-effective-target scheduling } */

int a, b, c, d, e, f;
int main() {
  if (e && f)
    while (1)
      while (a)
        a = 0;
  if (c) {
    if (b)
      goto g;
    int h = a;
  i:
    b = ~((b ^ h) | 1 % b);
    if (a)
    g:
      b = 0;
  }
  if (d)
    goto i;
  return 0;
}
