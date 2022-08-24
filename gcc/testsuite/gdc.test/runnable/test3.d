// PERMUTE_ARGS: -unittest -O -release -inline -fPIC -g
// EXTRA_SOURCES: imports/test3a.d imports/test3b.d

import imports.test3a;

extern(C) int printf(const char*, ...);

class Foo
{
    string bar;

    unittest
    {
        printf("in Foo.unittest()\n");
    }
}


void test(int a)
{
}

void test(uint b)
{
}


int main(string[] args)
{
    Foo a = new Foo;
    string baz = "lolo";

    test(3);
    a.bar = "hello";
    a.bar = baz ~ "betty";

    printf("a.bar = '%.*s'\n", cast(int)a.bar.length, a.bar.ptr);
    assert(a.bar == "lolobetty");
    return 0;
}
