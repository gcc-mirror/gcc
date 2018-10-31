// EXTRA_SOURCES: imports/a17a.d

module a17;

import std.stdio;

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
