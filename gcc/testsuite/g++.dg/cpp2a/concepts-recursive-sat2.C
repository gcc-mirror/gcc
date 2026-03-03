// { dg-do compile { target c++20 } }

template<typename T>
concept Fooable = requires(T t) { foo(t); };
// { dg-error "T = test::S]' depends on itself" "" { target *-*-* } .-1 }

template<Fooable T>
void foo(T t) { }

void test()
{
  struct S {} s;
  foo(s); // { dg-error "no match" }
}
