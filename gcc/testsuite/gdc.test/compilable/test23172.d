// https://issues.dlang.org/show_bug.cgi?id=23172

enum E : ubyte { // `ubyte` is needed to trigger the bug
    A,
    B,
}

struct S {
    E e;
}

void compiles(bool b, S s) {
    E e = b ? E.A : s.e;
}

void errors(bool b, const ref S s) {
    E e = b ? E.A : s.e;
}

// from https://issues.dlang.org/show_bug.cgi?id=23188

enum Status : byte
{
    A, B, C
}

Status foo()
{
    Status t = Status.A;
    const Status s = t;

    return (s == Status.A) ? Status.B : s;  // <-- here
}
