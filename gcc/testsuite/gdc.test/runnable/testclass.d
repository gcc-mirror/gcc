extern(C) int printf(const char*, ...);

/******************************************/
// 12078

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
// 12143

class Node12143
{
    alias typeof(true ? Node12143.init : Class12143.init) V;
    static assert(is(V == Node12143));
}

class Type12143 : Node12143 {}

class Class12143 : Type12143 {}

/***************************************************/
// 13353

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
// 15733

class CStmt15733 : CNode15733 {}
class CDecl15733 : CStmt15733 {}
class CNode15733 { mixin CMix!CDecl15733; }
template CMix(T){ mixin("static " ~ T.stringof ~ " x;"); }

interface IStmt15733 : INode15733 {}
interface IDecl15733 : IStmt15733 {}
interface INode15733 { mixin IMix!IDecl15733; }
template IMix(T){ mixin("static " ~ T.stringof ~ " x;"); }

/***************************************************/

int main()
{
    printf("Success\n");
    return 0;
}
