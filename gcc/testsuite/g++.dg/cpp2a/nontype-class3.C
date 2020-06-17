// { dg-do compile { target c++20 } }

struct A {
  int i;
  // auto operator<=> (const A&) = default;
};
template <A a> void g();
template <auto t> void f()
{
  g<t>();
}

int main()
{
  f<A{1}>();
}
