// https://issues.dlang.org/show_bug.cgi?id=20012

mixin template mixinFoo() {

    extern(C) void cFoo() {}

    extern(C) int cVar;
    extern(D) int dVar;

    void dFoo() {}

    mixin(`mixin mixinBar;`); // test nesting and interaction with string mixins
}

mixin mixinFoo;

mixin template mixinBar() {
    extern(C) void cBar() {}
    void dBar() {}
}

static assert(cFoo.mangleof == "cFoo");
static assert(dFoo.mangleof == "_D21mixinTemplateMangling8__mixin54dFooFZv");
static assert(cVar.mangleof == "cVar");
static assert(dVar.mangleof == "_D21mixinTemplateMangling8__mixin54dVari");
static assert(cBar.mangleof == "cBar");
static assert(dBar.mangleof == "_D21mixinTemplateMangling8__mixin5Qj4dBarFZv");

struct S {
    mixin mixinFoo;
    static assert(cFoo.mangleof == "_D21mixinTemplateMangling1S8__mixin14cFooMUZv");
    static assert(cBar.mangleof == "_D21mixinTemplateMangling1S8__mixin18__mixin54cBarMUZv");
    static assert(dBar.mangleof == "_D21mixinTemplateMangling1S8__mixin18__mixin54dBarMFZv");
    static assert(dFoo.mangleof == "_D21mixinTemplateMangling1S8__mixin14dFooMFZv");
}
