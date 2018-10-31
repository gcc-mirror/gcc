struct S
{
    int opApply(int delegate(string) dg)
    {
        return 0;
    }
}
void main()
{
    foreach (_; S())
    {
        return;
    }
}
