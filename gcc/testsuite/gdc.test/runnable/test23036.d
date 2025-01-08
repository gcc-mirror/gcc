// https://issues.dlang.org/show_bug.cgi?id=23036

struct S
{
    this(ref S) {}
    this(S, int a = 2) {}
}

void main()
{
    S a;
    S b = a;
}
