/*
TEST_OUTPUT:
---
fail_compilation/ice12235.d(14): Error: forward reference to inferred return type of function `__lambda1`
fail_compilation/ice12235.d(15): Error: forward reference to inferred return type of function `__lambda1`
fail_compilation/ice12235.d(15):        while evaluating `pragma(msg, __lambda1.mangleof)`
---
*/

void main()
{
    (){
        int x;
        enum s = __traits(parent, x).mangleof;
        pragma(msg, __traits(parent, x).mangleof);
    }();
}
