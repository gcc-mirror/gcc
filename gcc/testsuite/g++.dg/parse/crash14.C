// { dg-do compile }
// Contributed by: Giovanni Bajo <giovannibajo at libero dot it>
// PR c++/14448: Fold constant initializers in templates

template <int> struct A
{
    A();
};


template<typename T> void foo(T)
{
  static const int n=1+1;
  A<n+1> a;
}

void bar()
{
    foo(0);
}
