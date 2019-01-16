enum E { i }

struct S1 { bool opCast(T)() { return true; } }
struct S2 { bool opCast(T)() { return true; } }

import a = core.stdc.stdio;

void main()
{
    with (E)            // exp == TOKtype
        assert(S1());   // Doesn't enclose in ScopeStatement
    assert(S1());

    with (a)            // exp == TOKimport
        assert(S2());   // Doesn't enclose in ScopeStatement
    assert(S2());
}
