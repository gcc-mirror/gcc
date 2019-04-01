// PR c++/89612
// { dg-do compile { target c++11 } }

template <typename> 
struct C {
  template <int N>
  friend int foo() noexcept(N);

  template <int N>
  friend int foo2() noexcept(N); // { dg-error "different exception" }
};

template <int N>
int foo() noexcept(N);

template <int N>
int foo2() noexcept(N + 1);

C<int> c;
