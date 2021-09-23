// PR c++/83476

int n;
template <int& V> struct A {};
template <int& V> void f(A<V>);
int main() {
  A<n> a;
  f(a);
}
