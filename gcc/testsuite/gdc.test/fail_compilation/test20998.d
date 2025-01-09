// https://issues.dlang.org/show_bug.cgi?id=20998
/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/test20998.d(76): Error: undefined identifier `invalid`
X x = { invalid, 2, "asd" };
        ^
fail_compilation/test20998.d(76): Error: too many initializers for `X` with 2 fields
X x = { invalid, 2, "asd" };
                    ^
fail_compilation/test20998.d(83): Error: cannot implicitly convert expression `"a"` of type `string` to `int`
X2 x2 = { ptr: null, "a", ptr: 2, 444 };
                     ^
fail_compilation/test20998.d(83): Error: duplicate initializer for field `ptr`
X2 x2 = { ptr: null, "a", ptr: 2, 444 };
                               ^
fail_compilation/test20998.d(83): Error: too many initializers for `X2` with 3 fields
X2 x2 = { ptr: null, "a", ptr: 2, 444 };
                                  ^
fail_compilation/test20998.d(90): Error: overlapping initialization for field `ptr` and `x`
X3 x3 = { ptr: null, "a", ptr: 2, 444 };
               ^
fail_compilation/test20998.d(90): Error: cannot implicitly convert expression `"a"` of type `string` to `int`
X3 x3 = { ptr: null, "a", ptr: 2, 444 };
                     ^
fail_compilation/test20998.d(90): Error: duplicate initializer for field `ptr`
X3 x3 = { ptr: null, "a", ptr: 2, 444 };
                               ^
fail_compilation/test20998.d(90): Error: too many initializers for `X3` with 3 fields
X3 x3 = { ptr: null, "a", ptr: 2, 444 };
                                  ^
fail_compilation/test20998.d(98): Error: field `X4.ptr` assigning to misaligned pointers is not allowed in a `@safe` function
    X4 x4 = { ptr: null, "a", 444, ptr: 2, true };
                   ^
fail_compilation/test20998.d(98): Error: cannot implicitly convert expression `"a"` of type `string` to `int`
    X4 x4 = { ptr: null, "a", 444, ptr: 2, true };
                         ^
fail_compilation/test20998.d(98): Error: too many initializers for `X4` with 2 fields
    X4 x4 = { ptr: null, "a", 444, ptr: 2, true };
                              ^
fail_compilation/test20998.d(102):        called from here: `test()`
auto e = test();
             ^
fail_compilation/test20998.d(104): Error: cannot implicitly convert expression `1` of type `int` to `void*`
X2 a5 = { ptr: 1, ptr: 2, ptr: 444, ptr: 555 };
               ^
fail_compilation/test20998.d(104): Error: duplicate initializer for field `ptr`
X2 a5 = { ptr: 1, ptr: 2, ptr: 444, ptr: 555 };
                       ^
fail_compilation/test20998.d(104): Error: duplicate initializer for field `ptr`
X2 a5 = { ptr: 1, ptr: 2, ptr: 444, ptr: 555 };
                               ^
fail_compilation/test20998.d(104): Error: too many initializers for `X2` with 3 fields
X2 a5 = { ptr: 1, ptr: 2, ptr: 444, ptr: 555 };
                                         ^
fail_compilation/test20998.d(107): Error: too many initializers for `X2` with 3 fields
X2 c6 = { null, 2, true, null };
                         ^
fail_compilation/test20998.d(116): Error: cannot implicitly convert expression `1` of type `int` to `immutable(char*)`
    immutable Struct iStruct = {1, &ch};
                                ^
fail_compilation/test20998.d(116): Error: too many initializers for `Struct` with 1 field
    immutable Struct iStruct = {1, &ch};
                                   ^
fail_compilation/test20998.d(120):        called from here: `test2()`
auto t = test2();
              ^
---
*/

struct X {
	void* ptr;
	int x;
}
X x = { invalid, 2, "asd" };

struct X2 {
	void* ptr;
	int x;
	bool y;
}
X2 x2 = { ptr: null, "a", ptr: 2, 444 };

union X3 {
    void* ptr;
    int x;
    bool y;
}
X3 x3 = { ptr: null, "a", ptr: 2, 444 };

int test() @safe
{
    align (1) struct X4 {
        void* ptr;
        int x;
    }
    X4 x4 = { ptr: null, "a", 444, ptr: 2, true };
    return 0;
}

auto e = test();

X2 a5 = { ptr: 1, ptr: 2, ptr: 444, ptr: 555 };
X2 b5 = { ptr: null, y: true };
X2 c5 = { x: 2, true, ptr: null };
X2 c6 = { null, 2, true, null };

struct Struct {
        char* chptr;
}

int test2()
{
    char ch = 'd';
    immutable Struct iStruct = {1, &ch};
    return 0;
}

auto t = test2();
