module test11051;

version (Safe)
{
    void main() @safe
    {
        enum E { A, B }
        E e = cast(E)-1;

        final switch (e)
        {
            case E.A: break;
            case E.B: break;
        }
    }
}
else
{
    void main()
    {
        enum E { A, B }
        E e = cast(E)-1;

        final switch (e)
        {
            case E.A: break;
            case E.B: break;
        }
    }
}
