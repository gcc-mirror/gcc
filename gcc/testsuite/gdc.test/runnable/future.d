/* PERMUTE_ARGS:
REQUIRED_ARGS: -verrors=simple
TEST_OUTPUT:
---
runnable/future.d(17): Deprecation: method `future.B.msg` implicitly overrides `@__future` base class method; rename the former
runnable/future.d(12):        base method `future.A.msg` defined here
---
 */

class A
{
    @__future char msg() { return 'a'; }
}

class B : A
{
    char msg() { return 'b'; }
}

class C : B
{
    override char msg() { return 'c'; }
}

class D : A
{
    override char msg() { return 'd'; }
}

int main()
{
    auto a = new A();
    assert(a.msg() == 'a');
    auto b = new B();
    assert(b.msg() == 'b');
    auto c = new C();
    assert(c.msg() == 'c');
    auto d = new D();
    assert(d.msg() == 'd');

    assert(b.A.msg() == 'a');

    auto ba = cast(A)b;
    assert(ba.msg() == 'a');

    auto da = cast(A)d;
    assert(da.msg() == 'd');
    return 0;
}
