// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-O3" }
// { dg-additional-options "-O3 -m32 -mno-sse" { target { i?86-*-* x86_64-*-* } } }

void *operator new(__SIZE_TYPE__, void *a) { return a; }
class b {
public:
  using c = int *;
  c e();
  c h();
};
template <typename d> class j : b {
public:
  void l() {
    for (auto f = h(), g = e(); f != g; ++f)
      new (f) d();
  }
};
class m {
public:
  enum i {};
  struct C {
    i : 8;
    i k : 8;
  };
};
class o {
  j<m::C> n;
  o();
};
o::o() { n.l(); }
