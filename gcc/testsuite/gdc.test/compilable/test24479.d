// https://issues.dlang.org/show_bug.cgi?id=24479

/*
TEST_OUTPUT:
---
1
2
---
*/

struct S
{
    @1
    S opBinary(string op: "-")(S rhs) const pure nothrow @nogc
    {
        return rhs;
    }
    @2
    S opBinary(string op: "*")(S dur) const pure nothrow @nogc
    {
        return dur;
    }
}

private enum hasExternalUDA(alias A) = is(A == External) || is(typeof(A) == External);

void foo()
{
    static foreach (t; __traits(getOverloads, S, "opBinary", true))
        static foreach(attr; __traits(getAttributes, t))
            pragma(msg, attr);

    static assert(__traits(getOverloads, S, "opBinary", true).length == 2);
    alias A = __traits(getAttributes, __traits(getOverloads, S, "opBinary", true)[1]);
}
