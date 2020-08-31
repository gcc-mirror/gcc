// https://bugzilla.gdcproject.org/show_bug.cgi?id=242
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S242
{
    enum M = S242();
    int a = 42;

    auto iter()
    {
        this.a = 24;
        return this;
    }
}

S242 test242()
{
    return S242.M.iter;
}

void main()
{
    assert(test242() == S242(24));
}
