// { dg-do compile }
// Unary plus/minus are not lvalues.

// In templates we require an instantiation to emit the diagnostic. This
//  is wrong and it is PR 18474.

int n;

void f(void)
{
  -n = 0;        // { dg-error "lvalue" }
  +n = 0;        // { dg-error "lvalue" }
}

template <int>
void g(void)
{
  -n = 0;        // { dg-error "lvalue" "" { xfail *-*-* } }
  +n = 0;        // { dg-error "lvalue" "" { xfail *-*-* } }
}
