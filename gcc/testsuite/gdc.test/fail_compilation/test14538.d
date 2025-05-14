
/*
TEST_OUTPUT:
---
fail_compilation/test14538.d(18): Error: return value `x ? cast(uint)this.fCells[x].code : 32u` of type `uint` does not match return type `Cell`, and cannot be implicitly converted
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
