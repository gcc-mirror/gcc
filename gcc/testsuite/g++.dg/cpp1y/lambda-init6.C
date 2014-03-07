// Test that simple captures are not named in the closure type, but
// initialized captures are named.
// { dg-do compile { target c++1y } }

int main()
{
  int i;
  auto lam = [i,j=42]{};
  lam.j;
  lam.j.foo;			// { dg-error "::j" }
  lam.i;			// { dg-error "no member" }
}
