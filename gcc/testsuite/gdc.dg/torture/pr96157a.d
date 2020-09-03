// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96157
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S
{
    @disable this(this); // triggers nrvo
    int v;
}

__gshared void* p;

S[1000] foo() nothrow
{
    typeof(return) d;
    p = &d;
    return d;
}

void main()
{
    auto d = foo();
    assert(p == &d);
}
