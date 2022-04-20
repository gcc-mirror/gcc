import core.attribute;

@mustuse struct S {}

void test()
{
    S a, b;
    a = b;
}
