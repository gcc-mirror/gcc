// PR c++/54835, DR 1518
// { dg-options "-std=c++0x" }

struct A
{
  explicit A(int = 42);
};

int main()
{
  A a1 = { };			// { dg-error "explicit" }
  A a2 = { 24 };		// { dg-error "explicit" }
}
