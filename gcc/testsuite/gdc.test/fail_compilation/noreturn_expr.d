/*
TEST_OUTPUT:
---
fail_compilation/noreturn_expr.d(10): Error: type `noreturn` is not an expression
---
*/

int v(e)()
{
    return e + 0;
}

int main()
{
    return v!(noreturn)();
}
