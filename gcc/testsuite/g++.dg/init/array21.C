// PR c++/27722

void foo()
{
  const int x[] = 0; // { dg-error "initializer" }
  ++x; // { dg-error "read-only|operand" }
}
