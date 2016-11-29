// Core 1715
// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

template<class T> struct S {
private:
  typedef int X;
  friend struct B;
};

struct B {
  template<class T> B(T, typename T::X);
};

struct D: B {
  using B::B;
};

S<int> s;
B b(s, 2); // Okay, thanks to friendship.
D d(s, 2); // Now OK as well.
