// Test typeid of multidimensional array with no bounds.
// { dg-do compile }

#include <typeinfo>

int main()
{
	const char *s = typeid(double[][]).name(); // { dg-error "bounds|confused" }
	return 0;
}
