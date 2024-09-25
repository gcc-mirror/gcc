// Core Issue #1331 (const mismatch with defaulted copy constructor)
// { dg-do compile { target c++11 } }

struct M
{
  M() = default;
  M& operator=(M&);
};

template<typename T> struct W
{
  W() = default;
  W& operator=(const W&) = default; // { dg-error "binding" }
// { dg-error "implicitly deleted" "" { target c++17_down } .-1 }
  T t;
};

int
main ()
{
  W<M> w1, w2;
  w1 = w2; // { dg-error "use of deleted function" }
}
