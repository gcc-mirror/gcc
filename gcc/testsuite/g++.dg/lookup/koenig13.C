// PR c++/42687
// DR 705

namespace N
{
    struct S { };
    void f(const S &) { }
}

void f(const N::S &) { }

int main()
{
    N::S v;
    (f)(v); // no ambiguity: ADL is prevented with (), only ::f is considered
}
