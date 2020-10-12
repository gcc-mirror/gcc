// https://bugzilla.gdcproject.org/show_bug.cgi?id=57
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S57
{
    int a;
    long b;
    // Doesn't happen for bigger structs
}

S57 bar57()
{
    return S57(4, 42);
}

void main()
{
    S57 s = bar57();
    assert (s is S57(4, 42));
}
