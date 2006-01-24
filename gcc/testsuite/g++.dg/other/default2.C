// PR c++/16829
// { dg-do "compile" }

template<typename T> void foo(T, int = 0, int) {}  // { dg-error "default" }

void bar()
{
  foo(0);
}
