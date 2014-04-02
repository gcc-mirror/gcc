// { dg-do compile { target c++11 } }

// Exercise some member alias templates ...

template<class T, class U> class A0 {};

template<class T>
struct A1 {
    template<class U> struct S {};
    template<class U> using AA0 = A0<T, U>;

  void f(A0<T, int>);

  void
  foo()
  {
    AA0<int> a;
    const AA0<int> b;
    f(a);
    f(b);
  }
};

void
bar()
{
    A1<int> a1;
    a1.foo();
    A1<int>::AA0<int> a1aa0;
    a1.f(a1aa0);
}

// ... some simple member alias ...
struct B {
    using A = int;
};

B::A a;

// ... and some simple alias

using Int = int;
