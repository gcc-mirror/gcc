struct S
{
    @disable this();
}

void main()
{
    S[] s;
    s = s ~ s;
}
