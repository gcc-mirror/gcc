/*
TEST_OUTPUT:
---
fail_compilation/trait_loc_err.d(13): Error: can only get the location of a symbol, not `trait_loc_err`
fail_compilation/trait_loc_err.d(14): Error: can only get the location of a symbol, not `stdc`
---
*/
module trait_loc_err;
import core.stdc.stdio;

void main()
{
    __traits(getLocation, __traits(parent, main));
    __traits(getLocation, __traits(parent, core.stdc.stdio));
}
