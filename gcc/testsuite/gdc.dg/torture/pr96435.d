// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96435
// { dg-do run }

@safe bool test96435()
{
    int[2] array = [16, 678];
    union U { int i; bool b; }
    U u;
    u.i = 0x81818181;
    assert(array[u.b] == 678); // { dg-warning "accessing overlapped field" }
    return u.b; // { dg-warning "accessing overlapped field" }
}

@safe void main()
{
    auto b = test96435();
    if (b)
        assert(true);
    if (!b)
        assert(false);
}
