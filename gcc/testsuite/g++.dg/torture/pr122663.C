// { dg-do compile }
// { dg-additional-options "-g" }

int a, b, c;
char bk(int, int);
void bl(int);
inline unsigned d() {
  bl(c);
  return bk(b, 0);
}
struct e {
  template <class bc> e(bc &);
};
int g(e) { return 0; }
unsigned h(e k) { return g(k); }
unsigned i(e k) { return h(k); }
inline unsigned j() {
  unsigned bh = i(a);
  bh += d();
  return bh;
}
void f() {
  j();
  j();
  __builtin_unreachable();
}
