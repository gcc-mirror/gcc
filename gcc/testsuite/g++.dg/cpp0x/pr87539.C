// PR c++/87539
// { dg-do compile { target c++11 } }
// { dg-options "-Os" }

namespace a {
template <typename b, typename = b, typename = int> class c;
class exception {};
template <typename d> struct e { d f; };
struct g { g(); g(const g &); };
}
template <class, class, class = int> class h;
template <typename, typename> struct i;
template <int j> struct aa { static const int k = j; };
struct ac { typedef a::e<int> l; };
struct ad;
template <int, typename m, typename> struct ae { typedef m l; };
template <typename m, typename n> struct ae<false, m, n> { typedef n l; };
template <typename m, typename n, typename p> struct o {
  typedef typename ae<m::k, n, p>::l l;
};
struct af { af(char *); };
template <typename> struct ag { ag(a::g = a::g()) {} };
template <typename ah, typename ai, typename aj, typename al>
struct i<a::c<ah, ai, aj>, al> { typedef ag<al> l; };
namespace ak {
template <typename am, typename an, typename ao> struct ap {
  typedef typename o<am, an, ao>::l ::l l;
};
template <typename = ad> struct aq { typedef ad l; };
}
template <typename ar> struct as {
  typedef char at;
  template <typename au, typename> static decltype(au(), at()) av(int);
  template <typename, typename> static int av(...);
  static const bool k = sizeof(av<ar, int>(0)) == 1;
};
template <typename ar> struct aw { static const bool k = as<ar>::k; };
template <class ar> struct ax { typedef aw<ar> l; };
template <typename ar> struct ay { typedef typename ax<ar>::l l; };
template <typename ar> struct az : ay<ar>::l {};
template <class ar, class> struct ba : aa<az<ar>::k> {};
template <class> struct bb : ak::ap<ba<int, int>, ak::aq<>, int> {};
template <typename> struct q : ba<bb<int>::l, int> {};
template <class, class, bool> class r;
template <class s, class t> struct r<s, t, false> {
  s operator*();
  s operator++();
};
template <class, class, class, class, class>
class bc : public r<a::e<h<int, int>>, int, q<int>::k> {};
template <class bd, class be, class bf, class u, class v, class bg, class w,
          class x, class bh, class bi>
int operator!=(bc<bd, be, bf, u, v>, bc<bg, w, x, bh, bi>);
template <class, class, class> struct h {
  typedef af bl;
  bc<int, int, int, ak::ap<aa<false>, int, ac>::l, int> begin();
  bc<int, int, int, ak::ap<aa<false>, int, ac>::l, int> end();
  template <class bm> bm bn() const;
  template <class> int bo() const;
  template <class bm> int bx(const bl &, const bm &) const;
  template <class> int bp(const bl &) const;
};
template <class bq, class br, class am>
template <class bm>
bm h<bq, br, am>::bn() const { typename i<a::c<int>, bm>::l(); return bm(); }
template <class bq, class br, class am>
template <class>
int h<bq, br, am>::bo() const { i<a::c<int>, int>::l(); bn<int>(); return 0; }
template <class bq, class br, class am>
template <class bm>
int h<bq, br, am>::bx(const bl &bs, const bm &) const { bp<int>(bs); return 0; }
template <class bq, class br, class am>
template <class>
int h<bq, br, am>::bp(const bl &) const { bo<int>(); return 0; }
char bt[] = "";
void
d()
{
  h<int, int> bu;
  for (auto bv : bu)
    try {
      bv.f.bx(bt, 0);
    } catch (a::exception) {
    }
}
