// https://issues.dlang.org/show_bug.cgi?id=9884
module issue9884;

const(int)[] data;

static this()
{
    data = new int[10];
    foreach (ref x; data) x = 1;
    data[] = 1;
}

struct Foo
{
    static const(int)[] data;

    static this()
    {
        this.data = new int[10];
        foreach (ref x; this.data) x = 1;
        this.data[] = 1;
    }
}

void main() {}
