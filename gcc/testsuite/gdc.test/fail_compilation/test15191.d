/* TEST_OUTPUT:
PERMUTE_ARGS: -dip1000
---
fail_compilation/test15191.d(31): Error: returning `&identity(x)` escapes a reference to local variable `x`
fail_compilation/test15191.d(37): Error: returning `&identityPtr(x)` escapes a reference to local variable `x`
fail_compilation/test15191.d(43): Error: cannot take address of `ref return` of `identityPtr()` in `@safe` function `addrOfRefTransitive`
fail_compilation/test15191.d(43): Error: returning `&identityPtr(x)` escapes a reference to local variable `x`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15191
// https://issues.dlang.org/show_bug.cgi?id=22519

@safe:
ref int foo(return ref int s)
{
    return s;
}

int* bar(return ref int s)
{
    return &foo(s);
}

ref int identity(ref return int x) {return x;}
ref int* identityPtr(ref return int* x) {return x;}

int* addrOfRefEscape()
{
	int x;
	return &identity(x);
}

int** addrOfRefSystem() @system
{
	int* x;
	return &identityPtr(x);
}

int** addrOfRefTransitive()
{
	int* x;
	return &identityPtr(x);
}

int gInt;
ref int getGlobalInt() {return gInt;}

int* addrOfRefGlobal()
{
	return &getGlobalInt();
}
