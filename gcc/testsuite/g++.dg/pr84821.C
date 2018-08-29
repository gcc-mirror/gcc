// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-fPIC" { target fpic } }

typedef struct a *b;
struct a {
  int c;
  unsigned d[];
};
int e, f, g, i, l, m;
struct o {
  long *h;
  int c;
  unsigned j;
  int k;
  long aa;
};
inline void p(o *r) { r->c = g; }
int *n, *ab;
inline bool s(o *r) {
  for (; r->aa == 0; r->aa = r->h[r->j])
    if (r->j >= r->c)
      return false;
  r->aa >>= 1;
  for (; r->aa;)
    r++;
  *n = r->k;
  return true;
}
class t {
public:
  t(int);
  operator b() { return q; }
  b q;
};
bool u, v, ac;
void fn3() {
  long w, x;
  o ad;
  bool ae;
  t af(i), ag(i);
  for (p(&ad); s(&ad);) {
    a *ah = af, *ai = ah, *aj = ag;
    x = ai->d[e] >> f & 1;
    ae = x;
    ai = aj;
    w = ai->d[e] >> f & 1;
    ac = w;
    if (v && u && w && x)
      ab[l] = ++m;
    u = x;
  }
  for (;;)
    ;
}
