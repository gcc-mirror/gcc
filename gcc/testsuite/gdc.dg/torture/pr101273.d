// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101273
// { dg-do run }

struct S101273
{
    int x;
    S101273* impl;
    this(int x)
    {
        this.x = x;
        this.impl = &this;
    }
    ~this() { }
}

S101273 makeS101273()
{
    return S101273(2);
}

S101273 nrvo101273()
{
    S101273 ret = makeS101273();
    return ret;
}

S101273 rvo101273()
{
    return makeS101273();
}

void main()
{
    auto nrvo = nrvo101273();
    assert(&nrvo is nrvo.impl);

    auto rvo = rvo101273();
    assert(&rvo is rvo.impl);
}
