// https://issues.dlang.org/show_bug.cgi?id=20717

/*
TEST_OUTPUT:
---
false
---
*/

pragma(msg, is(typeof({
    struct S
    {
        struct Foo {}
        struct Bar() {}
        alias Bar = Foo;
    }
})));
