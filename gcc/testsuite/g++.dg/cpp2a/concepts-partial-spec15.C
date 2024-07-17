// PR c++/111890
// { dg-do compile { target c++20 } }

template<class>
struct A {
  template<class T>
  struct B { };

  template<class T> requires T::value
  struct B<T> { };

  template<class T> requires (sizeof(T) == sizeof(int))
  struct B<T> {
    friend void operator+(B&, int) { }
  };
};

void f(A<int>::B<int> b) {
  b + 0;
}
