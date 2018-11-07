/*
TEST_OUTPUT:
---
fail_compilation/diag11756.d(15): Error: cannot read uninitialized variable cnt in CTFE
fail_compilation/diag11756.d(34):        called from here: foo.ptr2.opAssign(Ptr(& n))
fail_compilation/diag11756.d(39):        called from here: test()
fail_compilation/diag11756.d(39):        while evaluating: `static assert(test())`
---
*/

struct Ptr
{
    void opAssign(Ptr other)
    {
        (*cnt)--;   // error
        cnt = other.cnt;
        (*cnt)++;
    }
    size_t *cnt;
}

union Foo
{
    size_t *ptr1;
    Ptr ptr2;
}

bool test()
{
    Foo foo;
    size_t cnt = 1;
    foo.ptr1 = &cnt;
    size_t n;
    foo.ptr2 = Ptr(&n);
    assert(cnt == 0);

    return true;
}
static assert(test());
