// { dg-do compile { target c++20 } }

template<typename T>
concept Fooable = requires(T t) { foo(t); };

template<Fooable T>
void foo(T t) { }

void test()
{
  foo(0); // { dg-error "no match" }
}
