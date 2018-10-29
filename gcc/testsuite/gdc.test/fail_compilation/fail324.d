/*
test_output:
---
fail_compilation/fail324.d(16): Error: template instance doStuff!((i){ return i; }) cannot use local '__lambda1' as parameter to non-global template doStuff(alias fun)()
---
*/

struct Foo
{
    void doStuff(alias fun)() {}
}

void main()
{
    Foo foo;
    foo.doStuff!( (i) { return i; })();
}
