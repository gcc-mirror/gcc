// { dg-do "compile" }
// { dg-options "-g" }
void pr117621()
{
    auto fun()(inout int)
    {
        struct S {}
        return inout(S)();
    }
    auto s = fun(0);
}
