// Test that a base with only a move constructor causes the implicit copy
// constructor to be deleted.
// { dg-do compile { target c++11 } }

struct A			// { dg-message "declares a move" }
{
  A();
  A(A&&);
};

struct B: A			// { dg-error "use of deleted" }
{
};

int main()
{
  B b1;
  B b2(b1);		    // { dg-error "deleted function .B::B.const" }
  B b3(static_cast<B&&>(b1));
}
