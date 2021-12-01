struct Thing
{
    this(int* i)
    {
        ptr = i;
        (*ptr)++;
    }

    ~this()
    {
        (*ptr)--;
    }

    T opCast(T : bool)()
    {
        return false;
    }

    int* ptr;
}

Thing makeThing(int* p)
{
    return Thing(p);
}

void main()
{
    int i;
    {
        if (auto t = makeThing(&i)) // destructor not called
        {
        }
    }
    assert(i == 0);
}
