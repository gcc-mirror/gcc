// https://bugzilla.gdcproject.org/show_bug.cgi?id=24
// { dg-do compile }

void test24()
{
    struct S24
    {
        char[1] b;
    }

    S24 a;

    if (*a.b.ptr)
        return;
}
