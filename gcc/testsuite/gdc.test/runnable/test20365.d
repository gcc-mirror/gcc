// https://issues.dlang.org/show_bug.cgi?id=20365

string result = "";

struct S
{
    long[3] a;
    this(ref typeof(this)) { result ~= "C"; }
}

void fun()
{
    S[4] a;
    auto b = a;
}

void main()
{
    fun();
    assert(result == "CCCC");
}
