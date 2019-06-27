// PR c++/90047
// { dg-do compile { target c++11 } }

template <int a> struct b { static constexpr int c = a; };
template <typename> struct aa;
template <typename...> struct d;
template <typename e, typename f, typename g, typename... h>
struct d<e, f, g, h...> : aa<e>::i {};
template <typename> struct j;
template <typename k, long l> struct j<k[l]> : b<true> {};
struct m {
  typedef b<0> i;
};
template <typename> struct n : m::i {};
template <bool> struct o;
template <typename p> struct aa { typedef p i; };
template <bool ab> using ac = typename o<ab>::i; // { dg-error "incomplete" }
class q {
  template <typename k, typename> using ad = ac<d<n<k>, int, int>::c>;
  template <typename k, typename = ad<k, void>> q(k &);
};
template <typename r> struct s {
  s(r) { t; }
  template <ac<!j<r>::c> *> void t();
};
class I {
  friend char operator<<(char p1, I p2) { return p1 << p2; }
  q ag;
};
int main() { s<char[10]> a = (char *)""; }
