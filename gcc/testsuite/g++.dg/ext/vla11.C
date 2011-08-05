// Test that auto works with VLAs.
// { dg-options -std=c++0x }

void bar(int n)
{
  float loc2[n];
  auto&& range = loc2;
}
