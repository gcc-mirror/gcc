/* { dg-do compile } */
/* { dg-options "-O2 -fPIE -fstack-protector-strong" } */

struct a {
  int b;
  int c();
  a() : b(c()) {}
  ~a();
  char *e();
};
struct f {
  void g(int);
};
struct ar {
  int au[256];
  f h(int);
} bb;
a i();
a j(int);
long k(int, ar);
int d;
void l(char *, ar m, long n) {
  switch (m.au[d])
  case 0:
    n &= 4294967295U;
  bb.h(0).g(n);
}
void o() {
  ar bd;
  a bh, bi, attrname = j(0) = i();
  int be = k(0, bd);
  l(attrname.e(), bd, be);
}
