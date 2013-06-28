// Test that error messages about creating the closure object refer to
// the lambda-introducer.
// { dg-options -std=c++0x }

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
