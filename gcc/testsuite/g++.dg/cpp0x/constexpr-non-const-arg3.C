// PR c++/49988
// { dg-options -std=c++0x }
// { dg-do run }

template<int ... I> struct X { };

struct A {
  char data[3];
  template<int ... I>
    constexpr
    A(const char (&s)[3], X<I...> x) : data{ s[I]...} { }
};
struct B {
  A a;
  B(const char (&s)[3]) : a{s,X<0,1,2>{}} { }
};

int main()
{
  B b{"12"};
  if (b.a.data[0] != '1')
    return 1;
}
