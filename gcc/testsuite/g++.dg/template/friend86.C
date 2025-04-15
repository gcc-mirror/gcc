// PR c++/119807
// { dg-do run }

template<int N>
struct A {
  template<class T> friend int f(A<N>, T);
};

template struct A<0>;
template struct A<1>;

int main() {
  A<0> x;
  A<1> y;
  if (f(x, true) != 0) __builtin_abort();
  if (f(y, true) != 1) __builtin_abort();
}

template<int N>
struct B {
  template<class T> friend int f(A<N>, T) { return N; }
};

template struct B<0>;
template struct B<1>;
