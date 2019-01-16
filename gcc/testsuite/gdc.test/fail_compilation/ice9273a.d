/*
TEST_OUTPUT:
---
fail_compilation/ice9273a.d(19): Error: constructor ice9273a.C.__ctor!().this no match for implicit super() call in constructor
fail_compilation/ice9273a.d(23): Error: template instance ice9273a.C.__ctor!() error instantiating
---
*/

template CtorMixin()
{
    this(T)() {}
}
class B
{
    mixin CtorMixin!();
}
class C : B
{
    this()() {}
}
void main()
{
    auto c = new C();
}
