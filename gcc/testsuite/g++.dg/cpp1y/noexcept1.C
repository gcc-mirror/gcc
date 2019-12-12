// PR c++/89873
// { dg-do compile { target c++14 } }

template <int> bool b;

template <typename> 
struct C {
  template <typename> friend int foo() noexcept(b<1>); // { dg-error "not usable in a constant expression|different exception specifier" }
};

template <typename> int foo() noexcept(b<1>);

auto a = C<int>();
