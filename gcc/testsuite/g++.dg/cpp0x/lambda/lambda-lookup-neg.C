// Test that we don't crash on a failed lookup.
// { dg-options -std=c++0x }

int main()
{
  [i]{};			// { dg-error "not declared" }
}
