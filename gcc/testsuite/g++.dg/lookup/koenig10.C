// Test for proper handling of class-scope enums.

struct A
{
  enum E { e };
  friend void f (E);
};

int main()
{
  f(A::e);
}
