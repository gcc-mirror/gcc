struct S
{
    template Temp(int x)
    {
        enum xxx = x;
    }
}

alias TT = __traits(getMember, S, "Temp");
enum x = TT!2.xxx;
static assert(x == 2);

class A
{
    mixin temp!("uint");
    mixin temp!("float");

    mixin template temp(string source)
    {
        private enum inner(string s) = s;
    }
}

class B
{
    alias member = __traits(getMember, A, __traits(allMembers, A)[0]);
}
