/* { dg-do compile } */

void f123(void) __attribute__((__returns_twice__));
int c;
volatile int i;
void e(long *a, long *d, int f) {
g:
  if (i)
    return;
  f123();
  for (;;) {
    __asm__ goto("" : : : : g);
    c = 0;
    for (; c < f; c++)
      a[c] = d[c];
  }
}
