// https://issues.dlang.org/show_bug.cgi?id=22639

struct A
{
    this(ref return scope A rhs) inout {}
    this(ref return scope const A rhs, int b = 7) inout
    {
        if (b != 7) {
            this.b = b;
        }
    }

    this(this) @disable;

    int a=4;
    int b=3;
}

void main()
{
    A a = A();
    A c = A(a, 10);
    A d = void;
    d.__ctor(a, 200);
    A* b = new A(a, 10);
}
