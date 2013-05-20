// PR c++/57327

template<class T>
struct A {};

template<class T>
void f(A<T>&) {}

struct B : A<long> {};

struct C : A<char> {};

struct D : B, C {};

int main()
{
  D d;
  f(d);  // { dg-error "no matching" }
}        // { dg-message "'A<T>' is an ambiguous base" "" { target *-*-* } 18 }
