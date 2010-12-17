// PR c++/46129
// The default argument for A<int>::B::operator() should not be instantiated

template <class T>
struct A {
  struct B {
    void operator () (const T& d_ = f(T()) ) { }
  };
};

int main() {
  A<int>::B b;
}
