// Test that captures are not named in the closure type.
// { dg-do compile { target c++14 } }

int main()
{
  int i;
  auto lam = [i,j=42]{};
  lam.j;                        // { dg-error "no member" }
  lam.i;			// { dg-error "no member" }
}
