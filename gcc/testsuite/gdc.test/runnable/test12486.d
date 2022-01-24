module test12486;

struct S { enum a = 1; } // or `const` but not for all types

S f(ref int i)
{
    ++i;
    return S();
}

void main()
{
    int i = 2;
    assert(f(i).a == 1);
    // ensure that f(i) was actually called, even though
    // a is a statically known property of the returned type
    assert(i == 3);
}
