// EXTRA_SOURCES: imports/ice15138a.d
// PERMUTE_ARGS: -unittest -inline
// COMPILE_SEPARATELY

import imports.ice15138a;

void main()
{
    JSONValue v;
    v.get!JSONValue;
}
