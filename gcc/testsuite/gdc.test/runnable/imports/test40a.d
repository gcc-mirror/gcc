module imports.test40a;

import std.stdio;

template Mix()
{
    static void foobar()
    {
	auto context = new Context;
        auto ts = context.toString;
	printf("context: %.*s %p\n", ts.length, ts.ptr, context);
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
    printf("str length: %d\n", z.length);
    printf("str: '%.*s'\n", z.length, z.ptr);
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

