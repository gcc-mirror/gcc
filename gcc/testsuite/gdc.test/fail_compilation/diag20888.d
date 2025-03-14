/*
TEST_OUTPUT:
---
fail_compilation/diag20888.d(24): Error: return value `callback` of type `int function()` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(24):        Did you intend to call the function pointer?
fail_compilation/diag20888.d(29): Error: return value `s` of type `string` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(34): Error: return value `callback` of type `int delegate()` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(34):        Did you intend to call the delegate?
fail_compilation/diag20888.d(39): Error: return value `callback` of type `int delegate()` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(39):        Did you intend to call the delegate?
fail_compilation/diag20888.d(44): Error: return value `callback` of type `int delegate()*` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(49): Error: return value `callback` of type `int delegate()` does not match return type `string`, and cannot be implicitly converted
fail_compilation/diag20888.d(54): Error: return value `() => 3755` of type `int function() pure nothrow @nogc @safe` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(54):        Did you intend to call the function pointer?
fail_compilation/diag20888.d(59): Error: `return` expression expected
fail_compilation/diag20888.d(64): Error: cannot return non-void from `void` function
fail_compilation/diag20888.d(70): Error: return value `() => i` of type `int delegate() pure nothrow @nogc @safe` does not match return type `int`, and cannot be implicitly converted
fail_compilation/diag20888.d(70):        Did you intend to call the delegate?
---
*/

int alpha(int function() callback)
{
	return callback;
}

int beta(string s)
{
	return s;
}

int gamma(int delegate() callback)
{
	return callback;
}

int delta(int delegate() callback)
{
	return callback;
}

int epsilon(int delegate()* callback)
{
	return callback; // no supplemental yet
}

string zeta(int delegate() callback)
{
	return callback;
}

int eta()
{
	return () => 0xEAB;
}

int theta()
{
	return;
}

void iota()
{
	return 0xEAB;
}

int kappa()
{
	int i = 0xEAB;
	return () { return i; };
}
