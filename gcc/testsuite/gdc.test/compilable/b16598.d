struct S
{
    this(int) {}
    ~this() {}
}

int g(S a, S b)
{
    return 1;
}

void main()
{
    true ? g(S(), S(1)) : {}();
}
