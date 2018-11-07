/*
TEST_OUTPUT:
---
fail_compilation/ctfe10989.d(11): Error: uncaught CTFE exception object.Exception("abc"c)
fail_compilation/ctfe10989.d(14):        called from here: throwing()
fail_compilation/ctfe10989.d(14):        while evaluating: `static assert(throwing())`
---
*/
int throwing()
{
    throw new Exception(['a', 'b', 'c']);
    return 0;
}
static assert(throwing());

/*
TEST_OUTPUT:
---
fail_compilation/ctfe10989.d(33): Error: uncaught CTFE exception object.Exception("abc"c)
fail_compilation/ctfe10989.d(36):        called from here: throwing2()
fail_compilation/ctfe10989.d(36):        while evaluating: `static assert(throwing2())`
---
*/
int throwing2()
{
    string msg = "abc";

    char[] arr;
    arr.length = msg.length;
    arr = arr[0 .. $];
    arr[] = msg;

    throw new Exception(cast(string)arr);
    return 0;
}
static assert(throwing2());
