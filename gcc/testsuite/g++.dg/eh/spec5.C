// Test for extension to allow incomplete types in an
// exception-specification for a declaration.

// { dg-do run }
// { dg-options "-fpermissive -w" }

struct A;

struct B
{
  void f ()
#if __cplusplus <= 201402L
  throw (A)
#endif
  ;
};

struct A {};

void B::f ()
#if __cplusplus <= 201402L
throw (A)
#endif
{}

int main ()
{
  B b;
  b.f();
}
