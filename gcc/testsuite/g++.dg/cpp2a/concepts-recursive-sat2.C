// { dg-do compile { target c++20 } }

template<typename T>
concept Fooable = requires(T t) { foo(t); }; // { dg-error "depends on itself" }

template<Fooable T>
void foo(T t) { }

void test()
{
  struct S {} s;
  foo(s); // { dg-error "no match" }
}
