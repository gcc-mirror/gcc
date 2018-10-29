module imports.link14588a;

void func(alias a)()
{
}

class A
{
    int i;

    void all()()
    {
        func!(i)();
    }
}
