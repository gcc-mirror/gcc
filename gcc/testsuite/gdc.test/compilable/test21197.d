/* REQUIRED_ARGS: -preview=dip1000
 */
// https://issues.dlang.org/show_bug.cgi?id=21197

@safe void check2()
{
    int random;

    S create1() return scope {
        return S();
    }

    scope S gen1 = create1;

    S create2() {
        return S(&random);
    }

    scope S gen2 = create2;
}

struct S
{
    int* r;
}
