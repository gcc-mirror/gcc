typedef char a;
a b, c;
int d, e;
void f(void *g) { *(volatile int *)g; }
void j() {
  a h, i;
  for (; b; b += 2) {
    d = b;
    i = i >> b;
    if (i)
      continue;
    f(&c + (b >> 2));
    h = 0;
    for (; h < 8 / 2; h++)
      if (i << h)
        e = 0;
  }
}
