// PR c++/33465

int foo(int);

void bar(double d)
{
  foo(d)();  // { dg-error "foo\\(\\(int\\)d\\)" }
}
