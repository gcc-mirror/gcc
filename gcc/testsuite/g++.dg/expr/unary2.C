// PR c++/18474
// { dg-do compile }
// Unary plus/minus are not lvalues.

int n;

void f(void)
{
  -n = 0;        // { dg-error "3:lvalue" }
  +n = 0;        // { dg-error "3:lvalue" }
}

template <int>
void g(void)
{
  -n = 0;        // { dg-error "lvalue" "" }
  +n = 0;        // { dg-error "lvalue" "" }
}
