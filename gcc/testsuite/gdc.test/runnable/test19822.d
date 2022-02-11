// https://issues.dlang.org/show_bug.cgi?id=19822
struct Quat
{
    static struct Vec { int x; }

    union
    {
        Vec v;
        struct { float x; }
    }

    static Quat identity()
    {
        Quat q;
        q.x = 1.0f;
        return q;
    }
}

struct QuatContainerWithIncompatibleInit
{
    Quat q = Quat.identity;
}

void main()
{
    QuatContainerWithIncompatibleInit c;
    assert(c.q.x == 1.0f); // fails
}
