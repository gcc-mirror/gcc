/* { dg-do compile } */                                                                        
/* { dg-options "-std=c++11 -O -fipa-cp -fipa-cp-clone --param=ipa-cp-max-recursive-depth=94 --param=logical-op-non-short-circuit=0" } */

enum a {};
struct m;
struct n {
  a d;
};
int o(int, int);
struct p {
  char d;
  char aa;
  p *ab;
  bool q() const {
    int h = d & 4;
    return h;
  }
  char r() const { return aa; }
  int s(const m *, bool) const;
} l;
struct t {
  p *ac;
  p *u() { return ac; }
  p *v(int);
};
int w(const p *, const p *, const m *, int = 0);
struct m : n {
  struct {
    t *ad;
  } ae;
  char x() const;
  p *y(int z) const { return ae.ad ? nullptr : ae.ad->v(z); }
} j;
int w(const p *z, const p *af, const m *ag, int ah) {
  int a, g = z->s(ag, true), i = af->s(ag, true);
  if (af->q()) {
    if (ag->x())
      return 0;
    ah++;
    char b = af->r();
    p *c = ag->y(b), *e = ag->ae.ad->u();
    int d = w(z, c, ag, ah), f = w(z, af ? e : af->ab, ag, ah);
    a = f ? d : f;
    return a;
  }
  if (g || i == 1)
    return ag->d ? o(g, i) : o(g, i);
  return 0;
}
void ai() {
  for (p k;;)
    w(&k, &l, &j);
}
