module imports.a13226;

enum isFunction(alias f) = is(typeof(f) == function);
enum isIntField(alias f) = is(typeof(f) == int);

string t(alias cls, string method)()
{
    static if (isFunction!(mixin("cls."~method)))
    {}
    return "";
}

string u(alias cls, string member)()
{
    static if (isIntField!(mixin("cls."~member)))
    {}
    return "";
}
