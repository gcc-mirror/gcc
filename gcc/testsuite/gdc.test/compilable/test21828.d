// https://issues.dlang.org/show_bug.cgi?id=21828

struct S
{
    enum E
    {
        e1 = 0,
    }
    E e;
    enum S s1 = S(E.e1);
}

SE se;

enum SE
{
    e1 = S.s1
}

// reduced case, forward references just assume int value

E e;

enum E
{
    a = "x"
}
