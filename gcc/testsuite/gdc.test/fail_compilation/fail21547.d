// https://issues.dlang.org/show_bug.cgi?id=21547

/*
TEST_OUTPUT:
---
fail_compilation/fail21547.d(34): Error: Cannot use struct initializer syntax for struct `Bar` because it has a constructor
fail_compilation/fail21547.d(34):        Use `Bar( arguments )` instead of `{ initializers }`
fail_compilation/fail21547.d(35): Error: Cannot use struct initializer syntax for struct `Bar1` because it has a constructor
fail_compilation/fail21547.d(35):        Use `Bar1( arguments )` instead of `{ initializers }`
---
*/

struct Bar
{
    @disable this(int a) {}
    this(int a, int b) {}

    string a;
    uint b;
}

struct Bar1
{
    @disable this(int a) {}
    this(const ref Bar1 o) {}
    this(int a, int b) {}

    string a;
    uint b;
}

void main ()
{
    Bar b = { a: "Hello", b: 42 };
    Bar1 b1 = { a: "Hello", b: 42 };
}
