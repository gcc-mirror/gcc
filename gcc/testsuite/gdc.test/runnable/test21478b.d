// https://github.com/dlang/dmd/issues/21478

// Test struct that implements copy constructor follows rvalue expression spec:
//      If both ref and non-ref parameter overloads are present,
//      an rvalue is preferably matched to the non-ref parameters,
//      and an lvalue is preferably matched to the ref parameter.
//      An RvalueExpression will preferably match with the non-ref parameter.

struct S21478
{
    this(ref return scope S21478) { assert(0); }
}

struct P21478
{
    int plain_old_data;
}

int overload(const S21478) { return 1; }
int overload(const ref S21478) { return 2; }

int overload(const P21478) { return 1; }
int overload(const ref P21478) { return 2; }

void main()
{
    S21478 s;
    assert(overload(s) == 2);
    assert(overload(S21478()) == 1);
    assert(overload(__rvalue(s)) == 1);

    P21478 p;
    assert(overload(p) == 2);
    assert(overload(P21478()) == 1);
    assert(overload(__rvalue(p)) == 1);
}
