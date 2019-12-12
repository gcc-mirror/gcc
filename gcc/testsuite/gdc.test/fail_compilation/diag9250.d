// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/diag9250.d(19): Error: cannot implicitly convert expression `10u` of type `uint` to `Foo`
fail_compilation/diag9250.d(22): Error: cannot implicitly convert expression `10u` of type `uint` to `void*`
---
*/

struct Foo
{
    ubyte u;
}

void main()
{
    uint[10] bar;

    Foo x = bar.length;  // error here

    void* y = bar.length ?
              bar.length :  // error here
              bar.length;
}
