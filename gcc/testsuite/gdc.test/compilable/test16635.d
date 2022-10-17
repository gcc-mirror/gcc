// https://issues.dlang.org/show_bug.cgi?id=16635

struct A
{
    alias get this;

    const(A) get() const
    {
        return A();
    }
}

static assert(!__traits(compiles, A() + A()));

// Original test (covers another path)

struct Vector2
{
    float x;
    float y;

    alias byRef this;

    ref const(Vector2) byRef() const
    {
        static Vector2 v;
        return v;
    }

    Vector2 opBinary(string op : "+")(ref const(Vector2) a) const
    {
        return Vector2(x + a.x, y + a.y);
    }
}

void test16635_1()
{
    Vector2 a = Vector2(1, 2);
    Vector2 b = Vector2(3, 4);

    // this line causes application to run infinitely
    // Already fixed. It was issue 16621
    Vector2 c = a + b;

    // OK <- this line seg faults without the above line
    Vector2 d = a + Vector2(5, 6);
}

void test16635_2()
{
    Vector2 a = Vector2(1, 2);
    Vector2 b = Vector2(3, 4);

    // just this line alone
    Vector2 d = a + Vector2(5, 6);
}
