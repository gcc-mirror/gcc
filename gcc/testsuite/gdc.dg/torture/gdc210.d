// https://bugzilla.gdcproject.org/show_bug.cgi?id=210
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S210
{
    ubyte a;
    uint b;
}

union U210
{
    S210 a;
    uint b;
}

S210 test210a()
{
    S210 s = S210(1, 2);
    return s;
}

S210[2] test210b()
{
    S210[2] s = [S210(1, 2), S210(3, 4)];
    return s;
}

U210 test210c()
{
    U210 s = U210(S210(1, 2));
    return s;
}

U210[2] test210d()
{
    U210[2] s = [U210(S210(1, 2)), U210(S210(3, 4))];
    return s;
}

void main()
{
    S210 a = S210(1, 2);
    assert(a == S210(1, 2));
    assert(a == test210a());
    assert(a != S210(2, 1));

    S210[2] b = [S210(1, 2), S210(3, 4)];
    assert(b == [S210(1, 2), S210(3, 4)]);
    assert(b == test210b());
    assert(b != [S210(2, 1), S210(3, 4)]);

    U210 c = U210(S210(1, 2));
    assert(c == U210(S210(1, 2)));
    assert(c == test210c());
    assert(c != U210(S210(2, 1)));

    U210[2] d = [U210(S210(1, 2)), U210(S210(3, 4))];
    assert(d == [U210(S210(1, 2)), U210(S210(3, 4))]);
    assert(d == test210d());
    assert(d != [U210(S210(2, 1)), U210(S210(3, 4))]);
}
