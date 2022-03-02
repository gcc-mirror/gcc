// PR c++/94944
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  void f();
};

template<class T>
struct B : A<T> {
  void g() noexcept(noexcept(A<T>::f()));
};

int main() {
  B<int> b;
  b.g();
}
