// Testcase for proper hiding of base conversion ops.

struct A
{
  operator const char *();
};

struct B : public A
{
  operator const char *() { return 0; }
};

int main( void )
{
  B b;
  const char *p = b;
}
