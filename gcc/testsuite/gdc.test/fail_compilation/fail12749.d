/*
TEST_OUTPUT:
---
fail_compilation/fail12749.d(19): Error: immutable field `inum` initialization is not allowed in foreach loop
fail_compilation/fail12749.d(20): Error: const field `cnum` initialization is not allowed in foreach loop
fail_compilation/fail12749.d(25): Error: immutable field `inum` initialization is not allowed in nested function `set`
fail_compilation/fail12749.d(26): Error: const field `cnum` initialization is not allowed in nested function `set`
---
*/
struct S
{
    immutable int inum;
    const     int cnum;

    this(int i)
    {
        foreach (n; Aggr())
        {
            inum = i;
            cnum = i;
        }

        void set(int i)
        {
            inum = i;
            cnum = i;
        }
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12749.d(48): Error: immutable variable `inum` initialization is not allowed in foreach loop
fail_compilation/fail12749.d(49): Error: const variable `cnum` initialization is not allowed in foreach loop
fail_compilation/fail12749.d(54): Error: immutable variable `inum` initialization is not allowed in nested function `set`
fail_compilation/fail12749.d(55): Error: const variable `cnum` initialization is not allowed in nested function `set`
---
*/
immutable int inum;
const     int cnum;
static this()
{
    int i = 10;

    foreach (n; Aggr())
    {
        inum = i;
        cnum = i;
    }

    void set(int i)
    {
        inum = i;
        cnum = i;
    }
}

struct Aggr
{
    int opApply(int delegate(int) dg) { return dg(1); }
}
