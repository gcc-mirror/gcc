interface IBar(T)
{
    IFoo!T ownerDocument();
}

interface IFoo(T): IBar!T
{
    // un-commenting the following line solves the issue
    //IList!T getList();
}

interface IList(T) {}

class DOMImplementation(T)
{
    class BarImpl: IBar!T
    {
        FooImpl ownerDocument() { return null; }
    }
    class FooImpl: BarImpl, IFoo!T
    {
        IList!T getList() { return null; }
    }
}

void main()
{
    auto impl = new DOMImplementation!string();
}
