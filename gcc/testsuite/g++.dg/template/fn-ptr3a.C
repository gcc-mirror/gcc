// PR c++/53164
// PR c++/105848
// A stricter version of fn-ptr3.C that verifies using f as a template
// argument alone constitutes an odr-use.

template<class T>
void f(T) { T::fail; } // { dg-error "fail" }

template<void (*P)(int)>
struct A {
  // P not called
};

template<void (&P)(char)>
void wrap() {
  // P not called
}

template<int>
void g() {
  A<f> a; // { dg-message "required from" }
  wrap<f>(); // { dg-message "required from" }
}

int main() {
  g<0>();
}
