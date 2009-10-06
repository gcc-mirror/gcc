// Test that capture by copy uses direct-initialization.
// { dg-options "-std=c++0x" }

struct A
{
  A();
  explicit A(const A&);
};

int main()
{
  A a;
  [a]{};
}
