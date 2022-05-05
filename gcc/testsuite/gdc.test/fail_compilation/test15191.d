/* TEST_OUTPUT:
REQUIRED_ARGS: -preview=dip1000
---
fail_compilation/test15191.d(35): Error: returning `&identity(x)` escapes a reference to local variable `x`
fail_compilation/test15191.d(41): Error: returning `&identityPtr(x)` escapes a reference to local variable `x`
fail_compilation/test15191.d(47): Error: cannot take address of `ref return` of `identityPtr()` in `@safe` function `addrOfRefTransitive`
fail_compilation/test15191.d(47):        return type `int*` has pointers that may be `scope`
fail_compilation/test15191.d(68): Error: cannot slice static array of `ref return` of `identityArr()` in `@safe` function `sliceOfRefEscape`
fail_compilation/test15191.d(68):        return type `int*[1]` has pointers that may be `scope`
---
*/

// Test taking the address of a `ref return` using & and [] operators
// https://issues.dlang.org/show_bug.cgi?id=15191
// https://issues.dlang.org/show_bug.cgi?id=22519
// https://issues.dlang.org/show_bug.cgi?id=22539

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

// Slice:
ref int*[1] identityArr(return ref scope int*[1] x)
{
	return x;
}

int* sliceOfRefEscape()
{
	int stackVar = 0xFF;
	scope int*[1] x = [&stackVar];
	int*[] y = identityArr(x)[];
	return y[0];
}
