/* PR target/85026.  */
/* { dg-do assemble } */
/* { dg-options "-O2 -std=gnu++11" } */

template <class> class a;
class b;
struct c {
  typedef a<b> &g;
};
template <typename d> struct e { typedef typename d::f iter; };
class h {
public:
  void __attribute__((noreturn)) i();
} ab;
template <class> class a {
public:
  typedef b *f;
  b &operator[](unsigned m) {
    if (ac)
      ab.i();
    return ad[m];
  }
  f n() { return ad; }
  f m_fn3();
  b *ad;
  unsigned ac;
};
class b {
public:
  short j;
  short k;
  signed l;
} __attribute__((__packed__));
void o(a<b> &m, b &p2, b &p) {
  p2 = p = m[0];
  if (bool at = false)
    ;
  else
    for (c::g au(m);; at = true)
      if (bool av = false)
        ;
      else
        for (e<a<int>>::iter aw = au.n(), ax = au.m_fn3(); ax;
             av ? (void)0 : (void)0)
          if (bool ay = 0)
            ;
          else
            for (b az = *aw; !ay; ay = true) {
              if (p2.j)
                p2.j = az.j;
              else if (p.j)
                p.j = az.j;
              if (p2.k)
                p2.k = az.k;
              else if (az.k > p.k)
                p.k = az.k;
              if (az.l < p2.l)
                if (az.l > p.l)
                  p.l = az.l;
            }
}
