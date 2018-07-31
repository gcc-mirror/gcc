// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-lto-options { { -fPIC -flto -g -shared } } }
class a {
    int b;
};
int const c = 0, d = 1, f = 2, g = 3;
struct B {
    typedef a h;
    h i;
};
template <class> B j();
template <class> struct k_context { static B const e_missingvar; };
template <class l> B const k_context<l>::e_missingvar = j<l>();
inline B m() {
    switch (0) {
      case c:
      case d:
      return k_context<int>::e_missingvar;
      case f:
      case g:;
    }
}
