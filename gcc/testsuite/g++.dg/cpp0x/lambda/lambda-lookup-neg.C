// Test that we don't crash on a failed lookup.
// { dg-do compile { target c++11 } }

int main()
{
  [i]{};			// { dg-error "not declared" }
}
