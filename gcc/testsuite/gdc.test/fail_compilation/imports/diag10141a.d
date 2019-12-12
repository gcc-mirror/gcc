module imports.diag10141a;

import imports.diag10141b;

string format()
{
    auto w = Appender!string();
    char[] digits = ['0'];
    put(w, digits);
    return null;
}

template Tuple(Specs...)
{
    struct Tuple
    {
        Specs expand;

        enum x = format();  // in injectNameFields()
    }
}

private template Identity(alias T)
{
    alias T Identity;
}
