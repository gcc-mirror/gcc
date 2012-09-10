// Core 1273
// { dg-do compile { target c++11 } }

template <class T> struct C;
template <class T> struct D;

class A
{
  int i;
  static int j;
  friend struct C<int>;
  friend struct D<int>;
} a;

class B
{
  int i;
  static int j;
  friend struct C<float>;
  friend struct D<float>;
} b;

template <class T>
struct C
{
  template <class U> decltype (a.i) f() { } // #1
  template <class U> decltype (b.i) f() { } // #2
};

template <class T>
struct D
{
  template <class U> decltype (A::j) f() { } // #1
  template <class U> decltype (B::j) f() { } // #2
};

int main()
{
  C<int>().f<int>();     // calls #1
  C<float>().f<float>(); // calls #2
  D<int>().f<int>();     // calls #1
  D<float>().f<float>(); // calls #2
}
