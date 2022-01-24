// https://issues.dlang.org/show_bug.cgi?id=20695
struct Bar
{
    this(const ref Bar o) {}

    string a;
    uint b;
}

struct Bar1
{
    @disable this(int a);
    this(const ref Bar1 o) {}

    string a;
    uint b;
}

struct Bar2
{
    this(const ref Bar2 o) {}
    @disable this(T)(T a) {}

    string a;
    uint b;
}
void main ()
{
    Bar b = { a: "Hello", b: 42 };
    Bar c = Bar("Hello", 42);

    Bar1 b1 = { a: "Hello", b: 42 };

    Bar2 b2 = { a: "Hello", b: 42 };
}
