/*
 * PERMUTE_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=17335

bool alwaysFalse() { return false; }
void main()
{
	static if (false && a == 1)
	{
	}
	static if ("a" == "b" && b == 1)
	{
	}
	static if (alwaysFalse() && c == 1)
	{
	}
	static if (!alwaysFalse() || d == 1)
	{
	}
	static if (alwaysFalse() ? e == 1 : 1)
	{
	}
	static if (!alwaysFalse() ? 1 : f == 1)
	{
	}
}
