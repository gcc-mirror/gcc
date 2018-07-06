// PR c++/84296

namespace b {}
namespace c {
using namespace b;
}
namespace b {
template <int d> struct e { static const int f = d; };
}
template <typename> struct g;
template <typename h, typename i, typename aa, typename j>
struct g<h(i, aa, j)> : h::template ab<i, aa, j> {};
struct k {
  template <typename l> struct m { typedef typename g<l>::n o; };
};
template <typename> struct ac;
struct r {
  typedef ac<int> p;
};
template <typename q> struct s : k {
  template <typename i, typename, typename>
  struct ab : q::template t<typename i::u>::template ab<i, int, int> {};
};
struct ad {
  typedef int u;
};
template <typename> struct ae;
template <typename, typename ag> struct ah {
  typedef ae<ag> ai;
  typedef typename ai::template w<ai(r, int)>::o n;
};
struct x {
  template <typename i, typename, typename> struct ab : ah<i, int> {};
};
struct y {
  struct z {
    template <typename> struct t : x {};
  };
  struct aj : s<z> {};
};
template <typename i> struct ak {
  typedef y::aj al;
  typedef typename al::m<al(i, int, int)>::o o;
};
struct am {
  enum { an };
};
template <typename> struct ao {};
template <typename af> struct ap : af::aq {};
template <> struct ae<int> {
  template <typename> struct w;
  template <typename ar, typename as, typename at> struct w<ar(as, at)> {
    typedef typename as::p o;
  };
};
enum { a = b::e<0>::f };
template <typename> class au;
template <typename av> struct ac : ao<av> { typedef c::e<am::an> aq; };
template <typename aw, typename i, typename ax> void ay(aw, i, ax) {
  au<c::e<ap<typename ak<i>::o>::f> > az();
}
void v() {
  ad a;
  void az();
  ay(az, a, v);
}
