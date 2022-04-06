// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/test32a.d
// PERMUTE_ARGS:

import imports.test32a;

void main()
{
    assert(S.sizeof == int.sizeof);
}
