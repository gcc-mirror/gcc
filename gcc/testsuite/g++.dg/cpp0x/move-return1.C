// PR c++/80452
// { dg-do compile { target c++11 } }

template<typename> struct check { };
template<typename T> struct check<T&>;

struct A {
  A() = default;
  A(A&&) = default;
  A(const A&) = delete;
};

template <class T>
struct B {
  template <class U> B(U&&) { check<U> u; }
};

B<A> f()
{
  A a;
  return a;
}
