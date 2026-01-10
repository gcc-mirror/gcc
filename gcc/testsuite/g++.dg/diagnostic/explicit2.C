// PR c++/115163
// { dg-do compile { target c++11 } }

struct A {
  explicit A(int);  // { dg-message "explicit conversion function was not considered" }
};
struct B {
  explicit operator int() const;  // { dg-message "explicit conversion function was not considered" }
};

int main() {
  A a = 42;  // { dg-error "conversion from .int. to non-scalar type .A." }
  int x = B{};  // { dg-error "cannot convert .B. to .int. in initialization" }
}
