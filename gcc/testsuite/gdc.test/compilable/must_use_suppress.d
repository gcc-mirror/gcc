import core.attribute;

@mustuse struct S {}

S fun() { return S(); }

void test()
{
    cast(void) fun();
}
