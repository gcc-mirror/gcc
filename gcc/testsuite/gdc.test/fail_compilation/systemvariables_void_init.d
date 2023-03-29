/**
REQUIRED_ARGS: -preview=systemVariables
TEST_OUTPUT:
---
fail_compilation/systemvariables_void_init.d(29): Error: `void` initializers for `@system` variables not allowed in safe functions
fail_compilation/systemvariables_void_init.d(30): Error: `void` initializers for `@system` variables not allowed in safe functions
fail_compilation/systemvariables_void_init.d(31): Error: `void` initializers for `@system` variables not allowed in safe functions
---
*/

struct S
{
	int x;
	@system int y;
}

struct C
{
	S[2] x;
}

enum E : C
{
	x = C.init,
}

void main() @safe
{
	S s = void;
	C c = void;
	E e = void;
}
