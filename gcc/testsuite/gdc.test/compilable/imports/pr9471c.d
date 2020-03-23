import imports.pr9471b;

struct Array(T)
{
    static if (is(typeof(T.opCmp))) { }
}
alias ClassDeclarations = Array!ClassDeclaration;

class Dsymbol
{
    void addObjcSymbols(ClassDeclarations);
}

class ScopeDsymbol : Dsymbol
{
    import imports.pr9471d;
    void importScope();
}
