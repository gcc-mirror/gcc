/*
TEST_OUTPUT:
---
fail_compilation/ice13921.d(13): Error: undefined identifier `undefined_identifier`
fail_compilation/ice13921.d(25): Error: template instance ice13921.S!string error instantiating
---
*/

struct S(N)
{
    void fun()
    {
        undefined_identifier;
        // or anything that makes the instantiation fail
    }

}

void test(T)(S!T)
{
}

void main()
{
    S!string g;
    test(g);
}
