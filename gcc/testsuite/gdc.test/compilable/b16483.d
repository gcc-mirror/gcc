struct S
{
    enum a = is(typeof(false.bar!(x => x))); // The lambda compiles
    enum b = is(typeof(false.bar!(x => y))); // The lambda doesn't compile
}
auto bar(alias foo)(bool var)
{
    return foo(var);
}
static assert(is(typeof(S.a) == bool));
static assert(S.a == true);
static assert(S.b == false);
