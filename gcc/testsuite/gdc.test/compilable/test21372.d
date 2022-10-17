// REQUIRED_ARGS: -de
struct S
{
    deprecated void foo(T)(int) { }
    void foo(T)(string) { }
}

// just to be safe, check this order too
// (there were some issues where naive checks of overloads were order dependent)
struct T
{
    void foo(T)(string) { }
    deprecated void foo(T)(int) { }
}

void main()
{
    // this should not hit the deprecation
    // because the parameter type doesn't match it
    S().foo!int("hi");

    // likewise
    T().foo!int("hi");
}
