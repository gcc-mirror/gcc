// https://issues.dlang.org/show_bug.cgi?id=20809


@safe:
struct S{
    @safe:
    int[8] a;
    ~this(){ a[] = 0; }
    ref val(){ return a; }
}
S bar(){ return S([2,2,2,2,2,2,2,2]); }
int[8] foo(){ return bar.val; }

void main(){ assert(foo() == [2,2,2,2,2,2,2,2]); } // error
