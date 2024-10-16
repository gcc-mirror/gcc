/* { dg-additional-options "-std=gnu17 -fanalyzer-transitivity" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

extern void g();
struct a {
} b(int c, int d) {
  struct a *e = 0;
  int f;
  if (c & 1 || !(c & 2))
    return *e;
  f = 0;
  for (; f < d - 1; f++)
    g(e[1]); /* { dg-warning "dereference of NULL" } */
}
