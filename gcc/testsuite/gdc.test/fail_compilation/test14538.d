// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/test14538.d(19): Error: cannot implicitly convert expression `x ? cast(uint)this.fCells[x].code : 32u` of type `uint` to `Cell`
---
*/

struct Cell
{
    dchar code;
    alias code this;
}

struct Row
{
    Cell[] fCells;
    Cell opIndex(size_t x) { return x ? fCells[x] : ' '; }
}
