// PR c++/32368
// { dg-options "-Wall -O" }

#include "pragma-system_header3.h"

int main()
{
	return f();
}
