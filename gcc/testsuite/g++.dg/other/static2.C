//PR c++/26573

void foo()
{
  struct A { static int i; };   // { dg-error "shall not have" }
}

template<typename T>
void bar()
{
  struct B { static int j; };   // { dg-error "shall not have" }
}
