// https://issues.dlang.org/show_bug.cgi?id=20821

struct S
{
    int gun()(int i) { return 0; }
    alias fun = gun;
    int fun() { return 1; }

    static int sgun()(int i) { return 0; }
    alias sfun = sgun;
    static int sfun() { return 1; }
}

static assert(S().fun == 1); // expressionsem.d changes
static assert(S.sfun == 1);  // ditto

static assert(__traits(getOverloads, S, "fun", true).length == 2); // traits.d changes
