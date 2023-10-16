/*
TEST_OUTPUT:
---
fail_compilation/diag3913.d(14): Error: no property `foobardoo` for type `Foo`
fail_compilation/diag3913.d(13):        enum `Foo` defined here
fail_compilation/diag3913.d(15): Error: no property `secon` for type `Foo`. Did you mean `Foo.second` ?
fail_compilation/diag3913.d(13):        enum `Foo` defined here
---
*/

void main()
{
    enum Foo { first, second }
    auto a = Foo.foobardoo;
    auto b = Foo.secon;
}
