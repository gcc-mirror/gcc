// https://issues.dlang.org/show_bug.cgi?id=19804

struct A { float e; }

void foo(A[1] a)
{
    void bar(A[1] a) { a[] = null; }
    bar(a);
}
