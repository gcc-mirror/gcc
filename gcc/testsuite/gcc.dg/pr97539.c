/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

int a, b;
void c() {
  char d;
  for (; b;)
    for (;;)
      for (; d <= 7; d += 1) {
        a = 7;
        for (; a; a += 1)
        e:
          d += d;
        d ^= 0;
      }
  goto e;
}
