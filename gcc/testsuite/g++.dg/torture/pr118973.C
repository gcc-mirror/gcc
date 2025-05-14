// { dg-do compile }

int foo() __attribute__((returns_twice));

void a()
{
  int a;
  if(foo()) new int;
  &a;
}
