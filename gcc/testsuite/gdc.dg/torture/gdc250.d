// https://bugzilla.gdcproject.org/show_bug.cgi?id=250
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void main()
{
    struct S
    {
        string data;
    }

    auto a = S("hello");
    auto b = S("hello".dup);

    assert(a.data == b.data);
    assert(a == b);
    assert([a] == [b]);
}
