// PERMUTE_ARGS:

import core.stdc.stdio;
import core.stdc.stdlib;

/*********************************************/

class Foo
{
    static uint flags;

    this()
    {
        printf("this() %p\n", this);
        a = 36;
    }

    ~this()
    {
        printf("~this() %p\n", this);
        a = -5;
        flags |= 1;
    }

    int a = 3;
    int b = 4;
    int d = 56;
}

void test1()
{
    Foo f;

    f = new Foo();
    assert(f.a == 36);
    assert(f.b == 4);
    assert(f.d == 56);
    assert(Foo.flags == 0);

    destroy(f);
    assert(Foo.flags == 1);
}

/*********************************************/
// delete is no longer a keyword and can be used as an identifier

enum E
{
    add, delete
}

E delete()
{
    return E.delete;
}

void test2()
{
    assert(delete() == E.delete);
}

/*********************************************/

int main()
{
    test1();
    test2();

    printf("Success\n");
    return 0;
}
