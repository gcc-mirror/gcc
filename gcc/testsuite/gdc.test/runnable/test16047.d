// https://issues.dlang.org/show_bug.cgi?id=16047
module test16047;

void main()
{
    Reassignable[int][int] aa;

    aa[0][0] = Reassignable.init;
    aa[s()][0] = Reassignable.init; // range violation
}

struct Reassignable
{
    void opAssign(Reassignable) {}
}

int s() { return 1; }
