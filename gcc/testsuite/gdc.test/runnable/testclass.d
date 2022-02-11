/*
RUN_OUTPUT:
---
Success
---
*/
extern(C) int printf(const char*, ...);

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12078

class B12078(T)
{
    static assert(is(T : B12078!T), "not related");
}
class D12078 : B12078!D12078
{
}

interface X12078(T)
{
    static assert(is(T : X12078!T), "not related");
}
interface Y12078 : X12078!Y12078
{
}

void test12078()
{
    static assert(is(D12078 : B12078!D12078));
    static assert(is(Y12078 : X12078!Y12078));
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12143

class Node12143
{
    alias typeof(true ? Node12143.init : Class12143.init) V;
    static assert(is(V == Node12143));
}

class Type12143 : Node12143 {}

class Class12143 : Type12143 {}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13353

interface Base13353(T)
{
    static assert(is(T : Base13353!T));
}

interface Derived13353 : Base13353!Derived13353
{
    void func();
}

class Concrete13353 : Derived13353
{
    void func() {}
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15733

class CStmt15733 : CNode15733 {}
class CDecl15733 : CStmt15733 {}
class CNode15733 { mixin CMix!CDecl15733; }
template CMix(T){ mixin("static " ~ T.stringof ~ " x;"); }

interface IStmt15733 : INode15733 {}
interface IDecl15733 : IStmt15733 {}
interface INode15733 { mixin IMix!IDecl15733; }
template IMix(T){ mixin("static " ~ T.stringof ~ " x;"); }

/***************************************************/

// https://issues.dlang.org/show_bug.zip?id=20716

extern(C++):

struct S20716
{
    void* s;
    ~this() {}
    // or this(this) {}
}

interface I20716
{
    S20716 x();
}

final class C20716 : I20716
{
    int l = 3;

    S20716 x()
    {
	//printf("this = %p, %p\n", this, &this.l);
        assert(l == 3); //fails
        return S20716.init;
    }
}

extern(D):

void test20716()
{
    auto s = new C20716().x;
    auto t = new C20716().I20716.x;
}

/***************************************************/

int main()
{
    test20716();

    printf("Success\n");
    return 0;
}
