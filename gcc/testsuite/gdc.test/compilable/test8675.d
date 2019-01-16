class MyError : Error
{
    this(string msg)
    {
        super(msg);
    }
}

void foo() nothrow
{
    throw new Error("Some error");
}

void bar() nothrow
{
    throw new MyError("Some error");
}
