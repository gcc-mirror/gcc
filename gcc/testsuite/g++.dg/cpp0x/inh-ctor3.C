// { dg-do compile { target c++11 } }
// { dg-options -fno-new-inheriting-ctors }

struct B1 {
  B1(int);
};
struct B2 {
  B2(int);
};
struct D1 : B1, B2 {
  using B1::B1;			// { dg-message "declared" }
  using B2::B2;			// { dg-error "inherited" }
};			   // ill-formed: attempts to declare D1(int) twice
struct D2 : B1, B2 {
  using B1::B1;
  using B2::B2;
  D2(int);    // OK: user declaration supersedes both implicit declarations
};
