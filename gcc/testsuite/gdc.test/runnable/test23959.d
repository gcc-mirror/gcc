// https://issues.dlang.org/show_bug.cgi?id=23959;

struct ST()
{
    int i;
    this(this) {}
}

alias S = ST!();

void poison()
{
    static S g;
    auto s = g;
}

S[1] sa;

void fun(S[] values...)
{
    sa[] = values;
}

int main()
{
    fun(S(1));
    assert(sa[0].i);

    return 0;
}
