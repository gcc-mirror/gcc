// DR 1391

template<class T> struct A {
  typename T::N n;
};
template<class T> struct B { };

template<class T, class T2>
void foo(const A<T>& r); // #1
template<class T>
void foo(const B<T>& r); // #2

void baz() {
  B<char> b;
  foo(b); // OK
  foo<char>(b); // error
}
