// REQUIRED_ARGS: -O -cov

bool func(T)()
{
    return true;
}

void main()
{
    assert(func!int() || int.sizeof);
}
