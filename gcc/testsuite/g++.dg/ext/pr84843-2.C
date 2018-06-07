// PR c++/84843
// { dg-do compile }
// { dg-options "" }

extern "C" int
__atomic_compare_exchange (int x, int y)	// { dg-error "ambiguates built-in declaration" }
{
  return x + y;
}
