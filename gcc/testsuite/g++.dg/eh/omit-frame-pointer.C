// { dg-do run }
// { dg-options -fomit-frame-pointer }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>

class Bug
{
};

int throw_bug()
{
	throw Bug();

	return 0;
}

int main()
{
	try {
		std::cout << throw_bug();
	} catch (Bug bug) {
	};
	
	return 0;
}
