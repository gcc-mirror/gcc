// PR c++/119233
// A version of fn-ptr3a.C using member instead of non-member function
// pointers.

struct B {
  template<class T>
  void f(T) { T::fail; } // { dg-error "fail" }
};

template<void (B::*P)(int)>
struct A {
  // P not called
};

template<void (B::*P)(char)>
void wrap() {
  // P not called
}

template<int>
void g() {
  A<&B::f> a; // { dg-message "required from" }
  wrap<&B::f>(); // { dg-message "required from" }
}

int main() {
  g<0>();
}
