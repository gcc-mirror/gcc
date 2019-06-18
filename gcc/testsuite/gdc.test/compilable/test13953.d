// https://issues.dlang.org/show_bug.cgi?id=13953

struct S
{
    string[string] aa;
    alias aa this;
}

void main()
{
    S s;
    s["foo"] = "bar";
    s.remove("foo");
}
