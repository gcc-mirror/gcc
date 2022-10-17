// Test for avoiding redundant scope qualifiers.

struct A
{
  struct B { };
  static void f(B,B);		// { dg-message {A::f\(B, B\)} }
};

int main()
{
  A::f(42);			// { dg-error "no match" }
}
