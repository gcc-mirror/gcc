// { dg-do compile }
// { dg-additional-options "-fpreview=in" }
struct S112290a
{
    S112290b* p;
    bool opEquals(in S112290a)
    {
        return p == p;
    }
}

struct S112290b
{
    string s;
}
