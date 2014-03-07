// Test that error messages about creating the closure object refer to
// the lambda-introducer.
// { dg-do compile { target c++11 } }

struct A
{
  A();
  A(const A& a) = delete;	// { dg-message "declared" }
};

int main()
{
  A ar[4][3];
  [ar] { };			// { dg-error "3:" }

  A a;
  [a] { };			// { dg-error "3:" }
}
