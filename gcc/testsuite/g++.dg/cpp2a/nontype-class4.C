// { dg-do compile { target c++20 } }

template <class T>
struct A {
  constexpr A(T) {}
  // auto operator<=> (const A&) = default;
};
template <A a> void f();

int main()
{
  constexpr A a = 1;
  f<a>();
  f<1>();
}

// { dg-final { scan-assembler "_Z1fIXtl1AIiEEEEvv" } }
// { dg-final { scan-assembler-not "_Z1fIXtlK1AIiEEEEvv" } }
