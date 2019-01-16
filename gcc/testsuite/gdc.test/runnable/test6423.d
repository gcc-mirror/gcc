// DISABLED: osx64

bool flag;

void f()
{
}

void main()
{
    if (!flag)
    {
        flag = true;
        caller();
    }
    return f();
}

alias maintype = extern(C) int function();

void caller()
{
    auto fp = cast(maintype)&main;
    assert(fp() == 0);
}
