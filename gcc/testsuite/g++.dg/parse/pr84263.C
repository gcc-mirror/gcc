// { dg-do compile { target c++11 } }
// { dg-additional-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" }
// PR 84263, a GC bug exposed on i686 native compiler (and possibly
// other 32-bit hosts).  decltype parsing could create a 
// pointer that would be gc-freed by later actions.

namespace std {
template <typename a> struct b {
  int c;
  a d;
};
template <typename> class g;
template <class> class initializer_list {
  void *e;
  __SIZE_TYPE__ f;
};
class h;
class j {
  typedef b<h> i;

public:
  j();
  j(initializer_list<i>);
};
template <typename> struct m;
template <int k> struct m<char[k]> {};
class h {
public:
  template <typename l> h(l &);
};
class G {
  G();
  j n;
};
G::G() { n = decltype(n){{0, ""}, {1, ".unoLineArrowEnd"}}; }
}
