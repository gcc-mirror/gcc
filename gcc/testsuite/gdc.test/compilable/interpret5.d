// https://issues.dlang.org/show_bug.cgi?id=21927
/*
TEST_OUTPUT:
---
T1(Args...)
T1!()
T2(Args2...)
T2!()
this.T2(Args2...)
this.T2!()
---
*/
template T1(Args...) {}

pragma(msg, T1);    // TOK.template_
pragma(msg, T1!()); // TOK.scope_

struct S
{
    template T2(Args2...) {}

    pragma(msg, S.T2);    // TOK.template_
    pragma(msg, S.T2!()); // TOK.scope_

    void fun()
    {
        pragma(msg, this.T2);    // TOK.dotTemplateDeclaration
        pragma(msg, this.T2!()); // TOK.dot
    }
}
