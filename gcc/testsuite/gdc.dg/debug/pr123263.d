// { dg-do compile }
void pr123263()
{
    struct UDA(T)
    {
        string name;
        T value;
    }
    @UDA!string() int k;
}
