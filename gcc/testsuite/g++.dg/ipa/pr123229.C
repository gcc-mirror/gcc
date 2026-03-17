/* { dg-do compile } */
/* { dg-options "-O3 -fno-exceptions" } */

class a {
  virtual int c();
};
struct e {
  virtual e *d(unsigned) const;
  void m_fn3() { d(0); }
} *f, *g;
struct h : a, e {
  e *d(unsigned) const { return i(); }
  virtual h *i() const;
};
struct j {
  virtual void k(e *);
};
void l(j &m) {
  m.k(g);
  while (1) {
    m.k(f);
    f->m_fn3();
  }
}
struct n : j {
  void k(e *m) {
    if (o)
      m->m_fn3();
  }
  bool o;
};
void p() {
  n b;
  l(b);
}

