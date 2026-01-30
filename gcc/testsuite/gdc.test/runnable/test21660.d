// REQUIRED_ARGS: -preview=bitfields
struct S
{
    int a : 3;
    int b : 3;
}

struct T
{
    uint a : 1;
    union {
        uint b : 2;
        struct {
            uint : 2;
            uint c : 3;
        }
    }
}

void main()
{
    S s = S(1, 2);
    assert(s.a == 1 && s.b == 2);
    S t = S(b:1, a:2);
    assert(t.a == 2 && t.b == 1);

    T u = T(1, 2, 3);
    assert(u.a == 1 && u.b == 2 && u.c == 3);
    T v = T(a:1, b:2, c:3);
    assert(v.a == 1 && v.b == 2 && v.c == 3);
}
