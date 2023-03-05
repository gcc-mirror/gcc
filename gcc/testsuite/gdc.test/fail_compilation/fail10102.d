/*
TEST_OUTPUT:
---
fail_compilation/fail10102.d(48): Error: variable `fail10102.main.m` - default construction is disabled for type `NotNull!(int*)`
fail_compilation/fail10102.d(49): Error: variable `fail10102.main.a` - default construction is disabled for type `NotNull!(int*)[3]`
fail_compilation/fail10102.d(50): Error: default construction is disabled for type `NotNull!(int*)`
fail_compilation/fail10102.d(51): Error: field `S.m` must be initialized because it has no default constructor
---
*/

struct NotNull(T)
{
    T p;

    alias p this;

    this(T p)
    {
        assert(p != null, "pointer is null");
        this.p = p;
    }

    @disable this();

    NotNull opAssign(T p)
    {
        assert(p != null, "assigning null to NotNull");
        this.p = p;
        return this;
    }
}

void main()
{
    struct S
    {
        NotNull!(int *) m;
        // should fail: an explicit constructor must be required for S
    }

    int i;
    NotNull!(int*) n = &i;
    *n = 3;
    assert(i == 3);
    n = &i;
    n += 1;

    NotNull!(int*) m;               // should fail
    NotNull!(int*)[3] a;            // should fail
    auto b = new NotNull!(int*)[3]; // should fail
    S s = S();                      // should fail
}
