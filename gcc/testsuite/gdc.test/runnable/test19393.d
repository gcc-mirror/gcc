string result;

struct S
{
    this(this)
    {
        result ~= "A";
    }

    ~this()
    {
        result ~= "B";
    }
}

void foo(const(S)[] ar...)
{
    /* postblit gets called on this initialization,
     * then when the function returns, the destructor
     * gets called => result = "AB";
     */
    auto d = ar[0];
}

void bar()
{
    /* S(null) needs to be destroyed after the function call,
     * that means that another `B` is appended => result = "ABB"
     */
    foo(S());
}

void main()
{
    bar();
    assert(result == "ABB");
}
