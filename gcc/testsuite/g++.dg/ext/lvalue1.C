// Test that we complain about the gcc cast-as-lvalue extension.

int main ()
{
  char c;

  static_cast<int>(c) = 2; // { dg-error "lvalue" "not an lvalue" { xfail *-*-* } }

  return c != 2;
}
