/* { dg-do compile } */
/* { dg-options "-w -O2 -fno-exceptions -std=c++11 -fpermissive" } */

namespace a {
template <typename b, b c> struct d { static constexpr b e = c; };
template <typename> struct f : d<bool, __is_trivially_copyable(int)> {};
}
typedef long g;
template <typename> struct h { static const bool e = a::f<int>::e; };
namespace a {
template <typename> struct ah;
template <typename> class ai;
}
class i {
public:
  operator[](long) const {}
};
template <typename, int> class am : public i {};
class an;
class k : public am<a::ai<an>, h<a::ai<a::ah<an>>>::e> {};
class l {
public:
  aq();
};
class ar extern as;
typedef k at;
class m {
  virtual bool av(int, unsigned &, at &, int &, g &, bool);
};
class ar {
public:
  typedef m *aw(const &, int &, const &, const &);
};
struct ax {
  static ay(ar::aw);
};
template <class az> struct n {
  n(ar) { ax::ay(ba); }
  static m *ba(const &bb, int &bc, const &bd, const &be) { az(bb, bc, bd, be); }
};
namespace {
class G : m {
  unsigned bi(const at &, l &);
  bool av(int, unsigned &, at &, int &, g &, bool);

public:
  G(const, int, const, const) {}
};
}
bool G::av(int, unsigned &, at &bl, int &, g &, bool) {
  l bo;
  bi(bl, bo);
}
o() { n<G> bp(as); }
namespace {
enum { bq, br };
}
unsigned G::bi(const at &bl, l &bo) {
  unsigned bs;
  for (char *j;; j += 2)
    switch (*j) {
    case bq:
      bl[bs];
    case br:
      bo.aq();
    }
}
