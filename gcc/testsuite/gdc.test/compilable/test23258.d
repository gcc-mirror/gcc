// https://issues.dlang.org/show_bug.cgi?id=23258

struct SumType(Types...)
{
    this(Types[0])
    {
    }
    this(Types[1])
    {
    }
}

alias A2 = SumType!(C1[], C2[]);

class C1
{
}

class C2
{
}
