/*
COMPILE_SEPARATELY:
EXTRA_SOURCES: imports/test38a.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
b = 49, 49
---
*/

import core.stdc.stdio;
import imports.test38a;

void main()
{
    static b = bar(7);
    printf("b = %d, %d\n", b, bar(7));
    assert(b == 49);
}
