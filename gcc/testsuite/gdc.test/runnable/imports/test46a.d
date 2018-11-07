module imports.test46a;

private import imports.test46c;

interface I
{
    void anything();
}

class A : I
{
    private C!(char) c;
    this() { c = new C!(char) (); }
    void anything() {}
}
