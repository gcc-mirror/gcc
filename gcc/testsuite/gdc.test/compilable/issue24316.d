struct S
{
    int i;
}

int f(immutable S *s)
{
    return s.i;
}

immutable S globalS = S(5);

static assert (f(&globalS) == 5);
