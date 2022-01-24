void delegate() foo()
{
    size_t value = 0;

    void check()
    {
        assert(value == 0);
    }

    void nest1()
    {
        void nest2() { check(); }
        nest2();
    }
    return &nest1;
}

void main()
{
    foo()();
}
