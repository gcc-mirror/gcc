struct S { enum int x = 42; }
static S dummy;
pure int fun(int x)
{
    return dummy.x + x;
}

void main()
{}
