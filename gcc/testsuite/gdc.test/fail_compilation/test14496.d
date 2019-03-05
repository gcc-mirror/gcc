/*
TEST_OUTPUT:
---
fail_compilation/test14496.d(21): Error: variable test14496.foo.f void initializers for pointers not allowed in safe functions
fail_compilation/test14496.d(24): Error: variable test14496.foo.Bar.foo void initializers for pointers not allowed in safe functions
fail_compilation/test14496.d(28): Error: variable test14496.foo.Baz.x void initializers for pointers not allowed in safe functions
fail_compilation/test14496.d(48): Error: variable test14496.sinister.bar void initializers for pointers not allowed in safe functions
fail_compilation/test14496.d(49): Error: variable test14496.sinister.baz void initializers for pointers not allowed in safe functions
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
}



