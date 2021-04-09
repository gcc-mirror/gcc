/* { dg-do compile { target { aarch64-*-* } } } */
/* { dg-skip-if "incorrect syntax for c++98" { *-*-* } { "-std=c++98" } { "" } } */
/* { dg-additional-options "-w -O3 -march=armv8.3-a -fdump-tree-vect-all" } */
/* { dg-require-effective-target le } */

typedef struct {
  float b;
  float c;
} d;
namespace {
typedef int e;
template <typename, typename> struct f;
template <template <typename> class g, typename h, typename k, typename... l>
struct f<g<k, l...>, h> {
  using m = g<h>;
};
} // namespace
namespace aa {
template <typename k> class o {
public:
  typedef k p;
};
} // namespace aa
namespace ab {
template <typename k> using r = aa::o<k>;
template <typename k> class ac : public r<k> {};
struct s {
  template <typename k, typename h> struct ad : f<k, h> {};
};
template <typename t, typename h> using ae = typename s::ad<t, h>::m;
template <typename t> struct af {
  typedef typename t::p p;
  template <typename k> using u = ae<t, k>;
};
} // namespace ab
namespace aa {
template <typename t> struct ag {
  typedef ab::af<t> v;
  typedef typename v::p &ah;
  template <typename k> struct ai { typedef typename v::u<k> aj; };
};
} // namespace aa
namespace ab {
template <typename k, typename t> struct w {
  typedef typename aa::ag<t>::ai<k>::aj x;
  struct y {};
  typedef t ak;
  w(e, ak);
  y a;
};
template <typename k, typename t = ac<k>> class al : w<k, t> {
  typedef w<k, t> am;
  typedef typename am::x x;
  typedef aa::ag<x> an;

public:
  typedef typename an::ah ah;
  typedef e ao;
  typedef t ak;
  al(ao ap, ak aq = ak()) : am(ar(ap, aq), aq) {}
  ah operator[](ao);
  ao ar(ao ap, ak) { return ap; }
};
} // namespace ab
void as(int n, d *a, d *q) {
  ab::al<d> z(n);
  d acc;
  for (int j = 0; j < n; ++j) {
    auto at = a[j];
    auto au = q[j];
    acc.b += at.b * au.b - at.c * au.c;
    acc.c += at.b * au.c + at.c * au.b;
  }
  z[0] = acc;
}


/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_MUL" 1 "vect" } } */
