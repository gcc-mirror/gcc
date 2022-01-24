// { dg-do run }
// { dg-additional-options "-fno-early-inlining" }
struct S101273
{
    int x;
    S101273* impl;
    S101273(int x)
    {
        this->x = x;
        this->impl = this;
    }
    S101273(const S101273 &o)
    {
	this->x = o.x;
	this->impl = this;
    }
    ~S101273() { }
};

S101273 makeS101273()
{
    return S101273(2);
}

S101273 nrvo101273()
{
    S101273 ret = makeS101273();
    return ret;
}

int main()
{
    auto nrvo = nrvo101273();
    if(&nrvo != nrvo.impl) __builtin_abort ();

    return 0;
}
