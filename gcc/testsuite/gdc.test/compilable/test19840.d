// https://issues.dlang.org/show_bug.cgi?id=19840
struct G
{
    ubyte[] I;
    alias I this;
}

auto M(ubyte[])
{
    G N;
    return N;
}

struct U { int V; }

void X()
{
    func((cast(U[])[].M));
}

void func(U[]) {}
