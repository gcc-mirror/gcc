// PR c++/47080
// { dg-options -std=c++11 }

struct A {
  explicit operator int();	// { dg-message "qualification conversion" }
};

int main() {
  bool b((A()));		// { dg-error "invalid user-defined" }
  !A();				// { dg-error "" }
}
