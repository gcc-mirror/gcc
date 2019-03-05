template binaryFun(alias fun)
{
    static if (is(typeof(fun) : string))
    {
        auto binaryFun(E1, E2)(E1 a, E2 b)
        {
            return mixin(fun);
        }
    }
    else
    {
        alias binaryFun = fun;
    }
}

class Foo(int n, alias greater = "a > b")
{
    private alias binaryFun!(greater) compare;

    public this()
    {
        //assert(compare(2, 1));
        // 'compare' -> FuncExp

        assert(this.compare(2, 1));
        // old: 'this.compare' -> DotVarExp (e1: ThisExp, var: FuncLiteralDeclaration)
        // new: 'this.compare' -> CommaExp (e1: ThisExp, e2: FuncExp)

        // Then, in both cases lambda->toObjFile() will run via FuncExp::toElem().
    }
}

void main()
{
    // OK
    auto foo1 = new Foo!(1, "a > b");
    auto foo2 = new Foo!(2, (a, b) => a > b);
    auto foo3 = new Foo!(3, delegate(a, b){ return a > b; });

    // OK <- NG
    auto foo4 = new Foo!(4, (int a, int b) => a > b);
    auto foo5 = new Foo!(5, function(int a, int b){ return a > b; });
    auto foo6 = new Foo!(6, delegate(int a, int b){ return a > b; });
}
