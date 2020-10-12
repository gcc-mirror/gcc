// https://bugzilla.gdcproject.org/show_bug.cgi?id=212
// { dg-do compile }

template hasElaborateAssign212(S)
{
    enum hasElaborateAssign212 = is(typeof(S.init.opAssign(rvalueOf212!S))) ||
        is(typeof(lvalueOf212!S)) ;
}

T rvalueOf212(T)();

T lvalueOf212(T)();


template TypeTuple212(TList...)
{
    alias TypeTuple212 = TList;
}

template Tuple212()
{
    struct Tuple212
    {
        void opAssign(R)(R)
        {
            if (hasElaborateAssign212!R)
            {
            }
        }
    }
}

ref emplaceRef212()
{
    static if (!hasElaborateAssign212!(Tuple212!()))
        chunk;
}

class TaskPool212
{
    void reduce()
    {
        Tuple212!() seed = void;
        Tuple212!()[] results;
        foreach(i; TypeTuple212!(0, 1))
            results[i] = seed;
    }
}
