// PR target/85196
// Testcase by Rainer Orth <ro@gcc.gnu.org>

// { dg-do compile }
// { dg-options "-O -fpermissive -w" }
// { dg-additional-options "-fPIC" { target fpic } }

class a;
template <typename> class b;
template <typename k> class d : public b<k> {};
class e {};
void f(int);
template <class> class g {
public:
  h();
  a i();
};
template <> class b<e> : public g<e> {};
typedef (*j)(d<e>);
template <class k> class l {
public:
  k operator->() { return 0; }
};
enum m { n, aa, o, ab, q, p };
inline s(m ac) {
  switch (ac) {
  case n:
  case aa:
  case p:
    return 1;
  case o:
  case ab:
    return 2;
  }
}
class D {
  int ad;

public:
  *ae() { return &ad; }
};
class a {
  l<D *> af;

public:
  *r() { return af->ae(); }
  t(int *c) {
    int *w = af->ae();
    return w == c;
  }
};
class F : a {
public:
  static int ah[];
  static e v(F *);
  unsigned long ai() const;
};
inline unsigned long F::ai() const {
  m aj = r() - &ah[0];
  return s(aj);
}
inline e F::v(F *ak) {
  long al = ak->ai();
  f(al);
}
template <typename> am() { return q; }
class an : public F {
public:
  static ao(d<e> u) {
    int *ap;
    m aq = am<unsigned>();
    ap = &ah[aq];
    return u.h() && u.i().t(ap);
  }
  template <e ar(F *)> static as() {
    F at;
    ar(&at);
  }
  template <e ar(F *)> static au(int *, unsigned, e *) {
    j av = ao;
    d<e> aw;
    if (av(aw))
      as<ar>();
  }
};
int *ax;
int ay;
e az;
ba() { an::au<an::v>(ax, ay, &az); }
