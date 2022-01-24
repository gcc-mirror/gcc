/*
TEST_OUTPUT:
---
fail_compilation/fail19871.d(10): Error: `struct Struct` may not define both a rvalue constructor and a copy constructor
fail_compilation/fail19871.d(19):        rvalue constructor defined here
fail_compilation/fail19871.d(13):        copy constructor defined here
---
*/

struct Struct
{
    @disable this();
    this(ref Struct other)
    {
        const Struct s = void;
        this(s);
    }

    this(Struct) {}
}
