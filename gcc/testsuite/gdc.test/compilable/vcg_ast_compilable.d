/*
REQUIRED_ARGS: -vcg-ast -o-
OUTPUT_FILES: compilable/vcg_ast_compilable.d.cg
TEST_OUTPUT:
---
=== compilable/vcg_ast_compilable.d.cg
import object;
auto binaryFun(E)(E b)
{
	return 'a' == b;
}
void find(Element)(Element needle) if (is(typeof(binaryFun(needle))))
{
}
void find()(string needle)
{
}
void splitter()
{
	find(3);
	find("");
}
binaryFun!int
{
	auto pure nothrow @nogc @safe bool binaryFun(int b)
	{
		return 97 == b;
	}

}
find!int
{
	pure nothrow @nogc @safe void find(int needle)
	{
	}

}
binaryFun!string
{
	auto _error_ binaryFun
	{
		__error__
	}

}
find!()
{
	pure nothrow @nogc @safe void find(string needle)
	{
	}

}
---
*/

// https://issues.dlang.org/show_bug.cgi?id=24431
auto binaryFun(E)(E b)
{
    return 'a' == b;
}

void find(Element)(Element needle) if (is(typeof(binaryFun(needle)))) { }
void find()(string needle) { }

void splitter()
{
    find!int(3);
    find!()("");
}
