module imports.imp21832;
static if(1)
{
    int fun(int a)
    {
        return a;
    }
    int tpl()(int a)
    {
        return a;
    }
}

deprecated
{
    int fun(char a)
    {
        return a;
    }
    int tpl()(char a)
    {
        return a;
    }
}
