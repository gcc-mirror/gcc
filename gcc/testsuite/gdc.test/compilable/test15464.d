class C15464
{
    static immutable field = 0;
}

struct S15464
{
    this(int i)
    {
    }
}

void issue15464(T)() @S15464(T.field)
{
}

void main()
{
    issue15464!C15464();
}


