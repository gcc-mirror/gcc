/*
TEST_OUTPUT:
---
fail_compilation/trait_loc_err.d(14): Error: can only get the location of a symbol, not `trait_loc_err`
fail_compilation/trait_loc_err.d(15): Error: can only get the location of a symbol, not `core.stdc`
fail_compilation/trait_loc_err.d(16): Error: can only get the location of a symbol, not `core.stdc.stdio`
---
*/
module trait_loc_err;
import core.stdc.stdio;

void main()
{
    __traits(getLocation, __traits(parent, main));
    __traits(getLocation, __traits(parent, core.stdc.stdio));
    __traits(getLocation, core.stdc.stdio);
}
