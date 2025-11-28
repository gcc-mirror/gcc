/* { dg-do compile } */
/* { dg-options "-march=armv8-a -std=c++11 -Ofast" } */

#include <vector>

int foo()
{
	bool xs[] = { true, true, false, true };
	bool s[] = { true, true, false };
	std::vector<bool> x(xs, xs+4);
	std::vector<bool> g(s, s+3);
	g.push_back(true);
      if (g != x)
      __builtin_abort();
	return 0;
}
