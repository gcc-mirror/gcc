// https://issues.dlang.org/show_bug.cgi?id=23083
int calls = 0;

int[2] f()
{
    calls++;
    return [123, 456];
}

void g(int a, int b) {}

void main()
{
    g(f().tupleof);
    assert(calls == 1);
}
