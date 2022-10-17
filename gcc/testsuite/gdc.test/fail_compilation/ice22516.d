/++
https://issues.dlang.org/show_bug.cgi?id=22516

TEST_OUTPUT:
---
fail_compilation/ice22516.d(18): Error: undefined identifier `X`
---
+/

struct Data
{
    void function() eval;

}

struct Builtins
{
    X x;

    Data myData = { (){} };
}
