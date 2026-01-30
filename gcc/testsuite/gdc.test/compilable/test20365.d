// REQUIRED_ARGS: -preview=bitfields

struct A { int x : 16, y : 16; }

void autoref_assign(A a)
{
    auto ref int x = a.x;
}

void f()(auto ref int);

void autoref_param(A a)
{
    f(a.y);
}

auto ref int autoref_return(ref A a)
{
    return a.y;
}
