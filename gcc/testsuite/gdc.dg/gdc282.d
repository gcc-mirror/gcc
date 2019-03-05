// https://bugzilla.gdcproject.org/show_bug.cgi?id=282
// { dg-shouldfail "conflicting methods in class" }
// { dg-do compile }

class C282a
{
    void fun()
    {
    }

    void f282()
    {
    }

    void f282() // { dg-error "conflicts with gdc282.C282a.f282" }
    {
    }
}

class C282b
{
    struct S282b
    {
    }

    void f282()
    {
    }

    void f282() // { dg-error "conflicts with gdc282.C282b.f282" }
    {
    }
}

class C282c
{
    class C282c
    {
    }

    void f282()
    {
    }

    void f282() // { dg-error "conflicts with gdc282.C282c.f282" }
    {
    }
}
