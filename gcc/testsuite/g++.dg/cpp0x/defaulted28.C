// PR c++/49102
// PR c++/50034
// { dg-do compile { target c++11 } }

struct A {
  A() = default;

private:
  A(A const&) = default;	// { dg-message "private" }
};

int f(...) { return 0; }
int main() {
  A a;
  f(a); 			// { dg-error "this context" }
  sizeof(f(a));			// OK because unevaluated
}
