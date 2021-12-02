// EXTRA_FILES: imports/test7491a.d imports/test7491b.d
struct Struct
{
    import object;
    import imports.test7491a;
    import renamed=imports.test7491b;
}

struct AliasThis
{
    Struct _struct;
    alias _struct this;
}

class Base
{
    import object;
    import imports.test7491a;
    import renamed=imports.test7491b;
}

class Derived : Base
{
}

interface Interface
{
    import object;
    import imports.test7491a;
    import renamed=imports.test7491b;
}

class Impl : Interface
{
}

static assert(__traits(compiles, Struct.object));
static assert(__traits(compiles, Struct.imports));
static assert(__traits(compiles, Struct.renamed));
static assert(__traits(compiles, AliasThis.object));
static assert(__traits(compiles, AliasThis.imports));
static assert(__traits(compiles, AliasThis.renamed));
static assert(__traits(compiles, Base.object));
static assert(__traits(compiles, Base.imports));
static assert(__traits(compiles, Base.renamed));
static assert(__traits(compiles, Derived.object));
static assert(__traits(compiles, Derived.imports));
static assert(__traits(compiles, Derived.renamed));
static assert(__traits(compiles, Interface.object));
static assert(__traits(compiles, Interface.imports));
static assert(__traits(compiles, Interface.renamed));
static assert(__traits(compiles, Impl.object));
static assert(__traits(compiles, Impl.imports));
static assert(__traits(compiles, Impl.renamed));
