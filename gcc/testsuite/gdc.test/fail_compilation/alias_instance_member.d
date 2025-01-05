/*
TEST_OUTPUT:
---
fail_compilation/alias_instance_member.d(18): Error: cannot alias member of variable `that`
fail_compilation/alias_instance_member.d(18):        Use `typeof(that)` instead to preserve behaviour
---
*/

@__edition_latest_do_not_use
module aim;

struct Foo
{
    int v;
    void test(Foo that) const
    {
        alias a = this.v; // OK
        alias b = that.v;
        assert(&a is &b);
    }
}

void main()
{
    Foo a = Foo(1);
    Foo b = Foo(2);
    a.test(b);
}
