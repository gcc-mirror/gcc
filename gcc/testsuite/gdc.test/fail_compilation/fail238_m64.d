// REQUIRED_ARGS: -m64
/*
TEST_OUTPUT:
---
fail_compilation/fail238_m64.d(21): Error: cannot implicitly convert expression `"a"` of type `string` to `ulong`
fail_compilation/fail238_m64.d(24): Error: cannot interpret X!() at compile time
fail_compilation/fail238_m64.d(29): Error: template instance fail238_m64.A!"a" error instantiating
fail_compilation/fail238_m64.d(35):        instantiated from here: M!(q)
fail_compilation/fail238_m64.d(35):        while evaluating pragma(msg, M!(q))
---
*/

// Issue 581 - Error message w/o line number in dot-instantiated template

template X(){}

template D(string str){}

template A(string str)
{
    static if (D!(str[str]))
    {}
    else
        const string A = .X!();
}

template M(alias B)
{
    const string M = A!("a");
}

void main()
{
    int q = 3;
    pragma(msg, M!(q));
}
