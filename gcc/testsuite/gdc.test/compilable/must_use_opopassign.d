import core.attribute;

@mustuse struct S
{
    ref S opOpAssign(string op)(S rhs) return
    {
        return this;
    }
}

void test()
{
    S a, b;
    a += b;
}
