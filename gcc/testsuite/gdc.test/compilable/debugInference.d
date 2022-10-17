/*
REQUIRED_ARGS: -debug
https://issues.dlang.org/show_bug.cgi?id=20507
*/


void main() pure  nothrow @safe @nogc
{
    debug foo();
    bar!()();
}

void foo() @system
{
    // Just to be sure its neither @nogc, pure or nothrow
    __gshared int counter = 0;

    if (counter++)
        throw new Exception(new immutable(char)[counter]);
}

void bar()()
{
    debug {
        foo();

        auto fPtr = &S.f;
        auto f2Ptr = &f2;

        S s;
        destroy(s);

        int* ptr = cast(int*) 0;
        int[] slice = ptr[0 .. 4];
        int val = ptr[1];

        void[] bytes = slice;
        bytes[] = bytes[];

        scope int n;
        int* pn = &n;
    }
}

class S {
    void f() @safe {}
}

ref int f2(return ref int i) {
    return i;
}
