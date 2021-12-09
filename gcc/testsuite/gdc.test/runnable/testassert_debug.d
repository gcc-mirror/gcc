/*
https://issues.dlang.org/show_bug.cgi?id=21598

REQUIRED_ARGS: -checkaction=context -debug
PERMUTE_ARGS:
*/

void main()
{
	bool caught;
	try
		assert(foo(1));
	catch (Throwable)
		caught = true;

	assert(caught);
	assert(counter == 1);
}

__gshared int counter;

int foo(int i) pure nothrow
{
	debug counter++;
	return i - 1;
}
