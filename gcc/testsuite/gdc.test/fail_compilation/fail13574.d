/*
TEST_OUTPUT:
---
fail_compilation/fail13574.d(21): Error: cannot modify operator `$`
fail_compilation/fail13574.d(27): Error: cannot modify operator `$`
---
*/

struct Foo
{
    void opSlice(size_t a, size_t b) {  }
    alias opDollar = length;
    size_t length;
}

void main()
{
    Foo foo;
    foo[0 .. foo.length = 1];
    assert(foo.length == 1);
    foo[0 .. $ = 2]; // assigns to the temporary dollar variable
    //assert(foo.length == 2);

    int[] arr = [1,2,3];
    auto x = arr[0 .. arr.length = 1];
    assert(arr.length == 1);
    auto y = arr[0 .. $ = 2]; // should also be disallowed
    //assert(arr.length == 2);
}
