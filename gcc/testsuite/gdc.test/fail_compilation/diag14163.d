/*
TEST_OUTPUT:
---
fail_compilation/diag14163.d(16): Error: constructor `diag14163.Bar.this` cannot call `super()` implicitly because it is annotated with `@disable`
---
*/

class Foo
{
    @disable this();
}

class Bar : Foo
{
    @disable this();
    this(int i) {}
}

void main() {}
