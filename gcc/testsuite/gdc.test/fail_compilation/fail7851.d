// https://issues.dlang.org/show_bug.cgi?id=7851
/*
TEST_OUTPUT:
---
fail_compilation/fail7851.d(38): Error: need `this` for `__mem_field_0` of type `int`
fail_compilation/fail7851.d(38): Error: need `this` for `__mem_field_1` of type `long`
fail_compilation/fail7851.d(38): Error: need `this` for `__mem_field_2` of type `float`
---
*/


template TypeTuple(TList...)
{
    alias TList TypeTuple;
}

struct Tuple(Specs...)
{
    TypeTuple!(int, long, float) mem;

    alias Identity!(mem[0]) _0;
    alias Identity!(mem[1]) _1;
    alias Identity!(mem[2]) _2;

    alias mem this;

    enum length = mem.length;
}

private template Identity(alias T)
{
    alias T Identity;
}


void main() {
  alias Tuple!(int, long, float) TL;
  foreach (i; TL)
  { }
}
