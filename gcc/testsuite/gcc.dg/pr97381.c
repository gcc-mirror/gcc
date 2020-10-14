// { dg-do compile }
// { dg-options "-O2" }

int a;
void b() {
  char c = 27;
  for (; c <= 85; c += 1) {
    a /= 148372120 * c;
    if (a)
      for (;;)
        ;
  }
}
