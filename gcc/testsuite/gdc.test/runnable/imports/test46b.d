module imports.test46b;

private import imports.test46c;

class B
{
    private C!(char) c;
    this() { c = new C!(char); }
}

