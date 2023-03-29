// https://issues.dlang.org/show_bug.cgi?id=23598

alias AliasSeq(a...) = a;

static if (1)
{

template sort(alias f, a...)
{
    static if (a.length > 0)
    {
        alias x = f!(a[0]);
        alias sort = a;
    }
    else
        alias sort = a;
}

alias SortedItems = sort!(isDependencyOf, String);

enum isDependencyOf(Item) = Item.DirectDependencies.length == 0;

struct String
{
    alias DirectDependencies = AliasSeq!();

    enum l = SortedItems.length; // (3)
}

}

/*****************************************************/

static if (1)
{
enum x = 1;
enum y = 2;

template f(T)
{
    alias b = int;
    static if (x)
    {
        alias c = x;
    }
    else
    {
        alias c = y;
    }

    static if (is(typeof(c)))
    {
    }
    else
    {
        static assert(0);
    }
}

void g()
{
    int x = f!int.c;
}
}

/*****************************************************/

template forward(args...)
{
    template fwd(alias arg)
    {
        alias fwd = arg;
    }

    alias Result = AliasSeq!();
    static foreach (arg; args)
        Result = AliasSeq!(Result, fwd!arg);
    static if (Result.length == 1)
        alias forward = Result[0];
    else
        alias forward = Result;
}

void func(int i, int j)
{
    func(forward!(i, j));
}
