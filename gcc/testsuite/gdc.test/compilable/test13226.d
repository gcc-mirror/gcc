// REQUIRED_ARGS: -o-
// PERMUTE_ARGS: -version=bug

import imports.a13226;

class C
{
    // class C member m is not accessible
    version(bug) mixin(t!(typeof(this), "f")); else {}
    version(bug) mixin(u!(typeof(this), "v")); else {}

    void f() {}
    int v;

    // here is ok
    version(bug) {} else mixin(t!(typeof(this), "f"));
    version(bug) {} else mixin(u!(typeof(this), "v"));
}

struct S
{
    // struct S member m is not accessible
    version(bug) mixin(t!(typeof(this), "f")); else {}
    version(bug) mixin(u!(typeof(this), "v")); else {}

    void f() {}
    int v;

    // here is ok
    version(bug) {} else mixin(t!(typeof(this), "f"));
    version(bug) {} else mixin(u!(typeof(this), "v"));
}
