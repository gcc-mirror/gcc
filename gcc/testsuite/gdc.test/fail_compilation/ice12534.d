/*
TEST_OUTPUT:
---
fail_compilation/ice12534.d(14): Error: static assert:  `is(exprs[0 .. 0])` is false
---
*/

alias TypeTuple(T...) = T;

void main()
{
    int x, y;
    alias exprs = TypeTuple!(x, y);
    static assert(is(exprs[0..0]));
}
