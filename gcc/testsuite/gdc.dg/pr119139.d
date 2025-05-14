// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=119139
// { dg-do compile }
// { dg-options "-fdump-tree-gimple" }
string toString()
{
    return "1";
}

struct B
{
    ulong n;

    invariant{}

    string str()
    {
        if (n == 0)
        {
            return "0";
        }
        return toString();
    }
}
// { dg-final { scan-tree-dump-not "static const struct  __result =" "gimple" } }
