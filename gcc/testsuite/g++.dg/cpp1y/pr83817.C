// PR c++/83817
// { dg-do compile { target c++14 } }

struct A;
struct B { template <typename> using C = A; };
struct D : B { struct F { typedef C<char> E; }; };
struct G {
  struct I { I (D, A &); } h;
  D::F::E &k ();
  D j;
  G (G &&) : h (j, k ()) {}
};
struct N { G l; };
typedef N (*M)(N &);
struct H { const char *o; M s; };
N foo (N &);
H r { "", [](auto &x) { return foo (x); }};
