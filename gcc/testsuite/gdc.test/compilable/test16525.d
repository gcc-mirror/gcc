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
    pragma(msg, templ!(D.memvar));
    pragma(msg, templ!(CPP.memvar));
    // root cause, C++ member variables have no mangling
    pragma(msg, CPP.memvar.mangleof);
}
