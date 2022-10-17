// https://issues.dlang.org/show_bug.cgi?id=22226

struct A {}

A move(A a) { return A.init; }

struct SumType
{
    A a;

    this(A value)
    {
        a = __ctfe ? value : move(value);
    }
}
