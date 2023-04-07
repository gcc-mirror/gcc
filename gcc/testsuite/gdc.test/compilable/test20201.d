/* REQUIRED_ARGS: -betterC
 */

// https://issues.dlang.org/show_bug.cgi?id=20101

public string ctfeHelper()(string a)
{
    return "int " ~ a ~ " = 42;";
}

extern(C) int main()
{
    int b = __traits(compiles, ctfeHelper("a"));
    mixin(ctfeHelper("a"));
    return !(a + b);
}
