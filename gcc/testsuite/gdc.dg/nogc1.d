// { dg-do compile }
// { dg-options "-fno-druntime" }
// { dg-shouldfail "expressions depend on GC" }

string testConcat(string a, string b)
{
    return a ~ b; // { dg-error "requires the GC and cannot be used with .-fno-druntime." }
}
