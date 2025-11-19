/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" { target { rv64 } } } */

long a;
void b() {
  unsigned long c, d;
  for (;; c = d + 2000) {
    d = c;
    for (; d < a; d += 2)
      if (d % 2)
        for (;;)
          ;
  }
}
