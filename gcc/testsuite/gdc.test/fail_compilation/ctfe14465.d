/*
TEST_OUTPUT:
---
fail_compilation/ctfe14465.d(19): Error: uncaught CTFE exception ctfe14465.E("message")
fail_compilation/ctfe14465.d(22):        called from here: foo()
fail_compilation/ctfe14465.d(22):        while evaluating: `static assert(foo())`
---
*/
class E : Exception
{
    this(string msg)
    {
        super(msg);
    }
}

bool foo()
{
    throw new E("message");
}

static assert(foo());
