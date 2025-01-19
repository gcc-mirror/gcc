/*
TEST_OUTPUT:
---
fail_compilation/fail19729.d(35): Error: `fail19729.C.__ctor` called with argument types `(string)` matches multiple overloads exactly:
fail_compilation/fail19729.d(18):     `fail19729.C.Templ!string.this(string t)`
and:
fail_compilation/fail19729.d(18):     `fail19729.C.Templ!string.this(string t)`
fail_compilation/fail19729.d(36): Error: `fail19729.D.__ctor` called with argument types `(string)` matches multiple overloads after qualifier conversion:
fail_compilation/fail19729.d(18):     `fail19729.D.Templ!(const(char)[]).this(const(char)[] t)`
and:
fail_compilation/fail19729.d(18):     `fail19729.D.Templ!(const(char)*).this(const(char)* t)`
---
*/
module fail19729;

mixin template Templ(T)
{
    this(T t) { }
}

class C
{
    mixin Templ!string;
    mixin Templ!string;
}

class D
{
    mixin Templ!(const(char)*);
    mixin Templ!(const(char)[]);
}

void main()
{
    new C("conflict");
    new D("conflict");
}
