// Test that initializing an aggregate with complex copy constructor
// and assignment ops doesn't cause cp_expr_size to abort.

struct A
{
  A();
  A(const A&);
  A& operator=(const A&);
};

struct B
{
  A a;
};

int main ()
{
  B b = { A() };
}
