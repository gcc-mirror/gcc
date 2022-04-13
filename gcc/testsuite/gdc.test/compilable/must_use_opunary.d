import core.attribute;

@mustuse struct S
{
    ref S opUnary(string op)() return
    {
        return this;
    }
}

void test()
{
    S s;
    ++s;
    --s;
    s++;
    s--;
}
