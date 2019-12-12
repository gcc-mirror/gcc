// PR c++/81866

template<class>
struct A {
  template<class> struct C;
  template<class> struct B;
  template<class T = B<int> > struct C {};
};

int main() {
  A<int>::C<> ac;
  return 0;
}
