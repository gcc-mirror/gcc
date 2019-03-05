// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/std11863conv.d imports/std11863format.d

import imports.std11863conv;

void main()
{
    auto s = to!string(15, 10);
    assert(s == "15");  // failure
}
