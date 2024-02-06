// PR c++/94231
// { dg-do compile { target c++11 } }

struct F {F(F&&)=delete;};

template<int=0>
struct M {
  F f;
  M();
  M(const M&);
  M(M&&);
};

template<int I>
M<I>::M(M&&)=default; // { dg-error "use of deleted function" }

M<> f() {
  M<> m;
  return m;
}
