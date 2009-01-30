// PR c++/33465

int foo(double);

void bar(int i)
{
  foo(i)();  // { dg-error "foo\\(\\(double\\)i\\)" }
}
