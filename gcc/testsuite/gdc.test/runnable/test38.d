// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/test38a.d
// PERMUTE_ARGS:

import std.stdio;
import imports.test38a;

void main()
{
    static b = bar(7);
    printf("b = %d, %d\n", b, bar(7));
    assert(b == 49);
}
