struct A {};

struct A foo()
{
  return foo();
}

void bar()
{
  foo();
}
