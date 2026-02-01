extern(C++) struct S
{
    static void link(alias MO)(){}
    __gshared int i;
}

void main()
{
    S.link!(S.i);
}
