// PR c++/84752
// { dg-do compile { target c++11 } }

void foo()
{
  constexpr int x[1] = {};
  [&x]{ return (bool)x; };
}
	
