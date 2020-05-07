// { dg-do link { target d_runtime_has_std_library } }

/******************************************/

class C1()
{
    static struct S1 { A1 a; }
}

enum E1 = is(C1!());

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=300

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

/******************************************/

void main() {}
