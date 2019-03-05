struct S
{
    ~this()
    {
        assert(false);
    }
}

void lazily(lazy S)
{
}

void main()
{
    lazily(S());
}
