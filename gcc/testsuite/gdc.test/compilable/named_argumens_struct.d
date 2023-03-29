
struct S
{
    string name;
    int x;
    int y;
}


immutable S s = S(x: 2, 3, name: "boo");

static assert(s.x == 2);
static assert(s.y == 3);
static assert(s.name == "boo");

union U
{
    float f;
    int i;
}

immutable U u = U(i: 2);

static assert(u.i == 2);
