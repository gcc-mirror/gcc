// https://issues.dlang.org/show_bug.cgi?id=20565

void main()
{
    {
        int temp(T)() { return 3; }
        assert(temp!int() == 3);
    }
    {
        int temp(T)() { return 4; }
        assert(temp!int() == 4);
    }
    {
        int temp(T)() { return 5; }
        assert(temp!int() == 5);
    }
}
