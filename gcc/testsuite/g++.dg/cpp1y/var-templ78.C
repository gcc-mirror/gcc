// PR c++/108468
// { dg-do compile { target c++14 } }

template<int> struct C {
  template<class, class>
  static constexpr int x = 1;
};

template<int I> template<class... A>
int C<I>::x<void(A...), void(A...)> = 2;

int y = C<0>::x<void(int), void()>;
