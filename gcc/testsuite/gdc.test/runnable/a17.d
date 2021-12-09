/*
EXTRA_SOURCES: imports/a17a.d
RUN_OUTPUT:
---
barx
---
*/

module a17;

import core.stdc.stdio;

private import imports.a17a;

class barx {
    this() { printf("barx\n"); }
}


int main()
{
    foo2x f = new foo2x();
//    f = new foo2x();
//    f.x++;

    return 0;
}
