// REQUIRED_ARGS: -preview=dip1000

void assertEq(scope int[][3] x) @safe
{
	bool b = x == x;
}
