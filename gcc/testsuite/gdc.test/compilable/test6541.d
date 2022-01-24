class C
{
    static synchronized func(alias a)() {}
}

void main()
{
    int a;
    C.func!a();
}
