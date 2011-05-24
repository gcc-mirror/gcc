// PR c++/49102
// { dg-options -std=c++0x }

struct A {
  A() = default;

private:
  A(A const&) = default;	// { dg-error "private" }
};

void f(...) { }
int main() {
  A a;
  f(a); 			// { dg-error "this context" }
}
