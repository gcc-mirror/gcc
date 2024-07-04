static struct S
{
    union
    {
        int i;
        long l;
    }
}

int f()
{
    S* r = new S();
    r.i = 5;
    return r.i;
}

enum X = f();
