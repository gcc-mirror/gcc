// Build don't link: 
// GROUPS passed error-messages
#include <fstream.h>
#include <iomanip.h>

// This error should not appear:
// bug.C: In method `test::test(const class test &)':
// bug.C:8: field `' not in immediate context

class test{
public:
	int	flags;
	test()	{};
	};

int main()

{
return 0;
}
