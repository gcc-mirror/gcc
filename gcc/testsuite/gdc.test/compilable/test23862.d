// https://issues.dlang.org/show_bug.cgi?id=23862

enum E { A, B }

void test(E e)
{
    with (e)
    switch (e)
    {
        case A:
        case B:
        default:
            break;
    }
}
