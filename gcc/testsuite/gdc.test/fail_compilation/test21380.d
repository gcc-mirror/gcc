// https://issues.dlang.org/show_bug.cgi?id=21380
/*
TEST_OUTPUT:
---
fail_compilation/test21380.d(39): Error: partial template instance `MySerializer().serializeSinkType!int` has no value
fail_compilation/test21380.d(44): Error: template instance `test21380.SupportSinkTypeSer!(MySerializer!int)` error instantiating
---
*/

template isSomeFunction(T...)
if (T.length == 1)
{
    static if (is(typeof(& T[0]) U : U*) && is(U == function) || is(typeof(& T[0]) U == delegate))
    {
        // T is a (nested) function symbol.
        enum bool isSomeFunction = true;
    }
    else static if (is(T[0] W) || is(typeof(T[0]) W))
    {
        // T is an expression or a type.  Take the type of it and examine.
        static if (is(W F : F*) && is(F == function))
            enum bool isSomeFunction = true; // function pointer
        else
            enum bool isSomeFunction = is(W == function) || is(W == delegate);
    }
    else
        enum bool isSomeFunction = false;
}

struct MySerializer (T)
{
	void serializeSinkType(T2) (scope auto ref T2 record) {}
}

template SupportSinkTypeSer(SerT)
{
    /* Note: Partial template instance because it needs inference, in this case
       it cannot infer 'auto ref' parameter */
	enum SupportSinkTypeSer = isSomeFunction!(SerT.init.serializeSinkType!int);
}

int main()
{
	enum x = SupportSinkTypeSer!(MySerializer!int);
	return 0;
}
