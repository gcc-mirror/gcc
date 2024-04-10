struct Line
{
    int line;
    alias line this;

    this(int line)
    {
        this.line = line;
    }
}

void foo(Line line1 = __LINE__, int line2 = __LINE__, int line3 = int(__LINE__))
{
    assert(line1 == 21);
    assert(line2 == 21);
    assert(line3 == 21);
}

void main()
{
    foo();
}
