// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for fstream" { ! hostedlib } }
// GROUPS passed error-messages
#include <fstream>
#include <iomanip>

// This error should not appear:
// bug.C: In method `test::test(const class test &)':
// bug.C:8: field `' not in immediate context

class test{
public:
	int	flags;
	test()	{}
	};

int main()

{
return 0;
}
