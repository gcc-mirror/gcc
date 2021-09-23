// PR c++/94492
// { dg-additional-options -Wdeprecated-copy }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-copy"
struct expr
{
    int a, b;
    expr& operator=(const expr&) { return *this; }
};
#pragma GCC diagnostic pop

expr foo(expr e)
{
    return e;
}
