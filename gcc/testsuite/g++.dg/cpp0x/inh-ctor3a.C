// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct B1 {
  B1(int);
};
struct B2 {
  B2(int);
};
struct D1 : B1, B2 {
  using B1::B1;
  using B2::B2;
};				// ambiguous
struct D2 : B1, B2 {
  using B1::B1;
  using B2::B2;
  D2(int);    // OK: user declaration supersedes both implicit declarations
};

D2 d2(42);
D1 d1(42);			// { dg-error "ambiguous" }
