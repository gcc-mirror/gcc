/*
TEST_OUTPUT:
---
fail_compilation/fail9346.d(26): Error: struct fail9346.S is not copyable because it is annotated with @disable
fail_compilation/fail9346.d(27): Error: struct fail9346.S is not copyable because it is annotated with @disable
---
*/

struct S
{
    @disable this(this);
}
struct SS1
{
    S s;
}
struct SS2
{
    S s;
    this(this){}
}

void main()
{
    S s;
    SS1 ss1 = SS1(s);
    SS2 ss2 = SS2(s);
}
