module imports.g313;

// adds public package imports (see https://issues.dlang.org/show_bug.cgi?id=15900)
public import imports.g313public;
// same w/ deferred semantics
static if (true)
    public import imports.g313staticif;
mixin("public import imports.g313stringmixin;");

template impD()
{
    public import imports.g313templatemixin;
}

mixin impD!();

void test15900()
{
    // publically imported modules should obviously be available in here as well
    imports.g313public.bug();
    imports.g313staticif.bug();
    imports.g313stringmixin.bug();
    imports.g313templatemixin.bug();
}
