// https://bugzilla.gdcproject.org/show_bug.cgi?id=43
// { dg-do compile }

void main()
{
    import core.vararg;
    import core.stdc.stdio;

    void formatArray(ref va_list argptr)
    {
        auto a = va_arg!(const(float)[])(argptr);
        foreach(f; a)
        {
            printf("%f\n", f);
        }
    }

    void doFormat(TypeInfo[] arguments, va_list argptr)
    {
        formatArray(argptr);
    }

    void format(...)
    {
        doFormat(_arguments, _argptr);
    }

    format([1.0f, 2.0f, 3.0f]);
}
