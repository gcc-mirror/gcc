// PR c++/53164
// PR c++/105848
// A C++20 version of fn-ptr3a.C using class NTTPs.
// { dg-do compile { target c++20 } }

template<class T>
void f(T) { T::fail; } // { dg-error "fail" }

struct B_int { void (*P)(int); };
struct B_char { void (&P)(char); };

template<B_int B>
struct A {
  // B.P not called
};

template<B_char B>
void wrap() {
  // B.P not called
}

template<int>
void g() {
  A<B_int{f}> a; // { dg-message "required from" }
  wrap<B_char{f}>(); // { dg-message "required from" }
}

int main() {
  g<0>();
}
