// { dg-do assemble  }
// GROUPS passed templates membertemplates
template<class T, int N>
class A { };

template<int N>
struct X {
    template<class T2, int N2>
    void f(A<T2,N>&, A<int,N2>&)
    { }
};


void foo()
{
  X<3> x;
  A<char*, 3> a1;
  A<int, 2> a2;
  x.f(a1, a2);
}
