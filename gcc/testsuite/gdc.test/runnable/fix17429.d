class Klazz
{
    __gshared size_t count;
    ~this()
    {
        ++count;
    }
}

void main()
{
    auto s = new Klazz;
    {
        scope s2 = s; // calls delete even though it does not own s
    }
    assert(Klazz.count == 0);
}
