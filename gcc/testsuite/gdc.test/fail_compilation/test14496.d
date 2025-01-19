/*
TEST_OUTPUT:
---
fail_compilation/test14496.d(21): Error: `void` initializing a pointer is not allowed in a `@safe` function
fail_compilation/test14496.d(24): Error: `void` initializing a pointer is not allowed in a `@safe` function
fail_compilation/test14496.d(28): Error: `void` initializing a pointer is not allowed in a `@safe` function
fail_compilation/test14496.d(48): Error: `void` initializers for pointers is not allowed in a `@safe` function
fail_compilation/test14496.d(49): Error: `void` initializers for pointers is not allowed in a `@safe` function
fail_compilation/test14496.d(50): Error: `void` initializers for pointers is not allowed in a `@safe` function
---
*/
// https://issues.dlang.org/show_bug.cgi?id=14496
@safe void foo()
{
    struct Foo {
        int* indirection1;
        Object indirection2;
        string[] indirection3;
    }

    Foo f = void;

    struct Bar {
        Foo foo = void;
    }

    struct Baz {
        int* x = void;
    }
}


struct Foo {
    int* indirection1;
    Object indirection2;
    string[] indirection3;
}

struct Bar {
    Foo foo = void;
}

struct Baz {
    int* x = void;
}

@safe void sinister() {
    Bar bar;
    Baz baz;
    Bar[2] bars; // https://issues.dlang.org/show_bug.cgi?id=23412
}
