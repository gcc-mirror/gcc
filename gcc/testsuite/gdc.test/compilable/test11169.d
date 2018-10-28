// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
1: false
2: true
3: true
---
*/

class A
{
    abstract void foo();
}

template MixinAbstractBar() { abstract void bar(); }

class B1 : A
{
    // Use pragma instead of static assert, in order to evaluate
    // __traits during ClassDeclaration.semantic().
    pragma(msg, "1: ", __traits(isAbstractClass, typeof(this)));
    override void foo() {}
}

class B2 : A
{
    pragma(msg, "2: ", __traits(isAbstractClass, typeof(this)));
    override void foo() {}
    abstract void bar();
}

class B3 : A
{
    pragma(msg, "3: ", __traits(isAbstractClass, typeof(this)));
    override void foo() {}
    mixin MixinAbstractBar!();
}

void main()
{
    static assert( __traits(compiles, { auto b = new B1(); }));
    static assert(!__traits(compiles, { auto b = new B2(); }));
    static assert(!__traits(compiles, { auto b = new B3(); }));
}
