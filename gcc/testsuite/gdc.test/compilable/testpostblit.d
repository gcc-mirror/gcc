struct Test1a
{
    this(this)
    {
    }
}

struct Test1b
{
    Test1a a;
}

struct Test1c
{
    const Test1b b;
    @disable this(this);
} 
