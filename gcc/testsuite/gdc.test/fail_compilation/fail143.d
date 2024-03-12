/*
TEST_OUTPUT:
---
fail_compilation/fail143.d(23): Error: calling non-static function `next` requires an instance of type `Quux`
fail_compilation/fail143.d(30): Error: template instance `fail143.Foo!int` error instantiating
---
*/

class Quux
{
    uint x;

    final uint next()
    {
        return x;
    }
}

template Foo(T)
{
    void bar()
    {
        int r = Quux.next;
    }
}

int main(char[][] args)
{
    auto prng = new Quux();
    alias Foo!(int).bar baz;

    int x = prng.next;
    baz();

    return 0;
}
