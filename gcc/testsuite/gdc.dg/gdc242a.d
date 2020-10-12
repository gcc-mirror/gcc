// https://bugzilla.gdcproject.org/show_bug.cgi?id=242
// { dg-do compile }

struct S242a
{
    enum M = S242a();
    void iter() { }
}

void test242a()
{
    return S242a.M.iter;
}

struct S242b
{
    enum M = S242b();
    void iter() { }
}

void test242b()
{
    S242b.M.iter;
}
