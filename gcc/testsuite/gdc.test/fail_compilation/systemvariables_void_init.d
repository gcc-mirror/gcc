/**
REQUIRED_ARGS: -preview=systemVariables
TEST_OUTPUT:
---
fail_compilation/systemvariables_void_init.d(48): Error: `void` initializers for types with unsafe bit patterns are not allowed in safe functions
fail_compilation/systemvariables_void_init.d(49): Error: `void` initializers for types with unsafe bit patterns are not allowed in safe functions
fail_compilation/systemvariables_void_init.d(50): Error: `void` initializers for types with unsafe bit patterns are not allowed in safe functions
fail_compilation/systemvariables_void_init.d(51): Error: a `bool` must be 0 or 1, so void intializing it is not allowed in safe functions
fail_compilation/systemvariables_void_init.d(52): Error: a `bool` must be 0 or 1, so void intializing it is not allowed in safe functions
fail_compilation/systemvariables_void_init.d(53): Error: `void` initializers for types with unsafe bit patterns are not allowed in safe functions
fail_compilation/systemvariables_void_init.d(54): Error: `void` initializers for types with unsafe bit patterns are not allowed in safe functions
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

enum B : bool
{
	x,
}

struct SB
{
	bool x;
}

struct SSB
{
	SB sb;
}

void main() @safe
{
	S s = void;
	C c = void;
	E e = void;
	const bool b = void;
	B bb = void;
	SB sb = void;
	SSB ssb = void;
}

// The following test is reduced from Phobos. The compiler generates this `opAssign`:
// (CopyPreventer __swap2 = void;) , __swap2 = this , (this = p , __swap2.~this());
// The compiler would give an error about void initialization a struct with a bool,
// but it can be trusted in this case because it's a compiler generated temporary.
auto staticArray(T)(T a) @safe
{
    T c;
    c = a;
}

void assignmentTest() @safe
{
    static struct CopyPreventer
    {
        bool on;
        this(this) @safe {}
        ~this() { }
    }

    staticArray(CopyPreventer());
}
