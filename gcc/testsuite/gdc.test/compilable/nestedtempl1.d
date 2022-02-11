class K
{
    class B(alias a)
    {

    }

    class D(alias a) : B!a
    {

    }

    class E(alias a) : B!1
    {

    }
}

void main()
{
    int a;
    auto k = new K;
    auto d = k.new K.D!a;
    auto e = k.new K.E!a;
}
