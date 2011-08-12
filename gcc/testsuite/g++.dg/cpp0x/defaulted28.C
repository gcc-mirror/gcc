// PR c++/49102
// PR c++/50034
// { dg-options -std=c++0x }

struct A {
  A() = default;

private:
  A(A const&) = default;	// { dg-error "private" }
};

int f(...) { }
int main() {
  A a;
  f(a); 			// { dg-error "this context" }
  sizeof(f(a));			// OK because unevaluated
}
