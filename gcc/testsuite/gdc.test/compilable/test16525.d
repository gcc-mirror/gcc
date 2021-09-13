static immutable templ(alias var) = 1234;

struct D
{
    int memvar;
}

extern(C++) struct CPP
{
    int memvar;
}

void test()
{
    static assert(templ!(D.memvar) == 1234);
    static assert(templ!(CPP.memvar) == 1234);
    // ICE: root cause, C++ member variables have no mangling
    enum CPPmemvar = CPP.memvar.mangleof;
}
