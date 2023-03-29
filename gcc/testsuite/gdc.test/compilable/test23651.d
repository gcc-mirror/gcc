// https://issues.dlang.org/show_bug.cgi?id=23651

template isCallable(alias callable)
{
    static if (is(typeof(&callable!())))
        enum bool isCallable = isCallable!(typeof(&callable!()));
    else
        enum bool isCallable = true;
}

string foo();

template FunctionTypeOf(alias func)
if (isCallable!func)
{
    alias FunctionTypeOf = typeof(foo);
}

template ReturnType(alias func)
{
    static if (is(FunctionTypeOf!func R == return))
        alias ReturnType = R;
}

template isAttrRange()
{
    alias NameType  = ReturnType!((string r) => r);
    //pragma(msg, "isAttrRange ", NameType, " ", string);
    static assert(is(NameType == string));

    enum isAttrRange = is(NameType == string);
}

static assert(isAttrRange!());
