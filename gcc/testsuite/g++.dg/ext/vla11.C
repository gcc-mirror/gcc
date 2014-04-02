// Test that auto works with VLAs.
// { dg-do compile { target c++11 } }
// { dg-options "" { target { ! c++1y } } }

void bar(int n)
{
  float loc2[n];
  auto&& range = loc2;
}
