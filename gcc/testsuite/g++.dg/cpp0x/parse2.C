// PR c++/54526
// { dg-do compile { target c++11 } }

template <class T>
struct X { };

struct A { };

int main()
{
  X<::A> x;
}
