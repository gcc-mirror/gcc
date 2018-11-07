// PERMUTE_ARGS:

import core.stdc.stdio;
import core.stdc.stdlib;

/*********************************************/

class Foo
{
    static uint flags;

    new(size_t sz, int x)
    {   void* p;

        printf("Foo.new(sz = %d, x = %d)\n", sz, x);
        assert(sz == Foo.classinfo.initializer.length);
        assert(x == 5);

        p = core.stdc.stdlib.malloc(sz);
        flags |= 4;
        return p;
    }

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

    delete(void* p)
    {
        printf("delete %p\n", p);
        free(p);
        flags |= 2;
    }

    int a = 3;
    int b = 4;
    int d = 56;
}

void test1()
{
    Foo f;

    f = new(5) Foo;
    assert(f.a == 36);
    assert(f.b == 4);
    assert(f.d == 56);
    assert(Foo.flags == 4);

    delete f;
    assert(Foo.flags == 7);
}


/*********************************************/

struct Foo2
{
    static uint flags;

    new(size_t sz, int x)
    {   void* p;

        printf("Foo2.new(sz = %d, x = %d)\n", sz, x);
        assert(sz == Foo2.sizeof);
        assert(x == 5);

        p = core.stdc.stdlib.malloc(sz);
        flags |= 4;
        return p;
    }

    delete(void *p)
    {
        printf("p = %p\n", p);
        flags |= 2;
        core.stdc.stdlib.free(p);
    }
}

void test2()
{
    Foo2 *f = new(5) Foo2();

    printf("f = %p\n", f);
    delete f;
    assert(Foo2.flags == 6);
}


/*********************************************/

int main()
{
    test1();
    test2();

    printf("Success\n");
    return 0;
}

