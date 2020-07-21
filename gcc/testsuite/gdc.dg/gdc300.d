// https://bugzilla.gdcproject.org/show_bug.cgi?id=300
// { dg-additional-options "-fmain" }
// { dg-do link { target d_runtime_has_std_library } }

struct S300(Range)
{
    double test(size_t remaining)
    {
        double r;
        return r ^^ remaining;
    }
}

auto link300a(Range)(Range)
{
    return S300!(Range)();
}

void link300()
{
    struct I {}
    static assert(is(typeof(link300a(I())) == struct));
    auto sample = link300a(I());
    sample.test(5);
}
