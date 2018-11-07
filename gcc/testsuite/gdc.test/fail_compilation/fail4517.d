
enum E : ushort
{
    A, B
}

void main()
{
    E e;
    final switch(e)
    {
        case E.A:
            break;
    }
}
