// Test that we complain about the gcc cast-as-lvalue extension.

int main ()
{
  char c;

  static_cast<int>(c) = 2; // { dg-error "3:lvalue" "not an lvalue" }

  return c != 2;
}
