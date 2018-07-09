// PR c++/86099
// { dg-do compile { target c++11 } }

template <int a> struct e { static constexpr int c = a; };
template <bool a> using d = e<a>;
template <bool, typename> struct aa;
template <typename...> struct j;
template <typename f, typename g> struct j<f, g> : aa<f::c, g>::h {};
template <typename i> struct n : d<i::c> {};
template <typename k, typename l = k> l m(int);
template <typename k> auto ab() -> decltype(m<k>(0));
template <typename...> struct p;
template <typename k, typename o> struct p<k, o> : e<noexcept(k(ab<o>()))> {};
template <typename> struct r;
class s;
template <typename, typename... q>
struct ac : j<d<true>, p<r<s>, q...>> {};
template <typename k> struct ae : ac<k, k> {};
template <bool, typename ad> struct aa { typedef ad h; };
template <typename k> struct w : j<n<ae<k>>, d<true>> {};
template <typename t> struct r {
  t f;
  int af;
  r(r &&) = default;
};
template <typename k, typename = typename aa<w<k>::c, k>::h> void v(k *);
template <typename ag, typename ah, typename ai> ah aj(ag x, ah, ai) { v(x); return 0; }
template <typename> struct y { typedef int ak; };
template <typename, typename = int> class z {
public:
  template <typename...> void al();
};
template <typename k, typename am> template <typename...> void z<k, am>::al() {
  r<s> *u;
  typename y<am>::ak a = aj(u, a, int());
}
class s {
  char *an = nullptr;
};
void ao() {
  z<int> b;
  b.al();
}
