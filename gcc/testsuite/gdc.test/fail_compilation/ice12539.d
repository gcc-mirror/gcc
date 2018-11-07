/*
TEST_OUTPUT:
---
fail_compilation/ice12539.d(15): Error: array index [0] is outside array bounds [0 .. 0]
---
*/

alias TypeTuple(E...) = E;

void main ()
{
    int[string] map;

    alias Foo = TypeTuple!();
    auto a = map[Foo[0]];
}
