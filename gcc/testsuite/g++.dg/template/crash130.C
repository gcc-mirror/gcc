// PR c++/62207

template<typename T> void foo(T)
{
  X;  // { dg-error "not declared" }
  X;
}

void X();
void X(int);

void bar()
{
  foo(0);
}
