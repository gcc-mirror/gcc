// https://bugzilla.gdcproject.org/show_bug.cgi?id=248
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

class C248b
{
    bool isintegral()
    {
        return false;
    }
}

class C248a
{
    int count = 0;

    C248b getMemtype()
    {
        count++;
        return new C248b();
    }
}

class C248
{
    C248a sym;

    this()
    {
        this.sym = new C248a();
    }

    bool isintegral()
    {
        return sym.getMemtype().isintegral();
    }
}

void main()
{
    C248 e = new C248();
    e.isintegral();
    assert(e.sym.count == 1);
}
