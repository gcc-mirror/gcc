// { dg-do compile }
// Unary plus (but not unary minus) can be applied to pointer types

void *p;

void f(void)
{
  -p;        // { dg-error "wrong type argument" }
  +p;
}

template <int>
void g(void)
{
  -p;        // { dg-error "wrong type argument" }
  +p;
}
