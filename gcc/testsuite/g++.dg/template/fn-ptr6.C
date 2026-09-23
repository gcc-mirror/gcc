// PR c++/126280
// A version of fn-ptr3a.C where the template parameter is a pack.
// { dg-do compile { target c++11 } }

template<class T>
void f(T) { T::fail; } // { dg-error "fail" }

template<void (*...P)(int)>
struct A {
  // P not called
};

template<void (&...P)(char)>
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
