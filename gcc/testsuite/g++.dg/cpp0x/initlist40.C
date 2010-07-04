// { dg-options "-std=c++0x" }

struct A
{
  explicit A(int = 42);
};

int main()
{
  A a1 = { };
  A a2 = { 24 };		// { dg-error "explicit" }
}
