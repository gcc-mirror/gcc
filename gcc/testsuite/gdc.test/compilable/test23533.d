// https://issues.dlang.org/show_bug.cgi?id=23533
// REQUIRED_ARGS: -preview=nosharedaccess

enum E { a, b }

void main()
{
	E x = E.b;
	E y = E.max;
}
