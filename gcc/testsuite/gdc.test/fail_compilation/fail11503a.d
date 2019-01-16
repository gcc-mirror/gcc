struct S
{
    immutable(S)* s;
    this(int) immutable pure
    {
        s = &this;
    }
    int data;
}

immutable(S)* makes() pure
{
    return new immutable S(0);
}

void main()
{
    S* s = makes(); // s is mutable and contains an immutable reference to itself
    //s.s.data = 7; // this is immutable
    s.data = 3; // but this is not!!!
}
