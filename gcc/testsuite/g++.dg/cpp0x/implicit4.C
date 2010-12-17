// Test that a base with only a move constructor causes the implicit copy
// constructor to be deleted.
// { dg-options "-std=c++0x" }

struct A
{
  A();				// { dg-message "A::A" }
  A(A&&);			// { dg-message "A::A" }
};

struct B: A			// { dg-error "implicit|no match" }
{
};

int main()
{
  B b1;
  B b2(b1);		    // { dg-error "deleted function .B::B.const" }
  B b3(static_cast<B&&>(b1));
}
