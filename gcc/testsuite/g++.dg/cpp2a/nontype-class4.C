// { dg-do compile { target c++20 } }
// { dg-additional-options "-fabi-version=18 -fabi-compat-version=18 -Wabi=0" }

template <class T>
struct A {
  constexpr A(T) {}
  // auto operator<=> (const A&) = default;
};
template <A a> void f();	// { dg-warning "mangled name" }

int main()
{
  constexpr A a = 1;
  f<a>();
  f<1>();
}

// { dg-final { scan-assembler "_Z1fIXtl1AIiEEEEvv" } }
// { dg-final { scan-assembler-not "_Z1fIXtlK1AIiEEEEvv" } }
