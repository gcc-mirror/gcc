module imports.test40a;

import core.stdc.stdio;

template Mix()
{
    static void foobar()
    {
	auto context = new Context;
        auto ts = context.toString;
	printf("context: %.*s %p\n", cast(int)ts.length, ts.ptr, context);
	context.func!(typeof(this))();
	printf("returning from opCall\n");
    }
}


class Bar
{
    mixin Mix;
}


void someFunc(string z)
{
    printf("str length: %zd\n", z.length);
    printf("str: '%.*s'\n", cast(int)z.length, z.ptr);
}


class Context
{
    void func(T)()
    {
	printf("<context.func\n");
	printf("thisptr: %p\n", this);
	someFunc(`a`);
	printf("context.func>\n");
    }
}
