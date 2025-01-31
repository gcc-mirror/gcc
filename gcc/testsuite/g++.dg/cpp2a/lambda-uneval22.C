// { dg-do compile { target c++20 } }

struct S {
  template <int I>
  using T = decltype([]{ return I; });

  template <int I>
  decltype([]{ return I; }) f();  // { dg-error "declared using local type" }
};

void a(S::T<0>*);  // { dg-error "declared using local type" }
void b(S::T<1>*);  // { dg-error "declared using local type" }
void c(decltype(S{}.f<0>())*);  // { dg-error "declared using local type" }
void d(decltype(S{}.f<1>())*);  // { dg-error "declared using local type" }

int main() {
  a(nullptr);
  b(nullptr);
  c(nullptr);
  d(nullptr);
  S{}.f<2>()();
}
