class C
{
    int m;
    auto fun()
    {
        inc!m();
        new N!m;
    }
}

auto inc(alias m)()
{
    ++m;
}

class N(alias m)
{
}

void main()
{
    auto c = new C;
    c.new N!(c.m);
}
