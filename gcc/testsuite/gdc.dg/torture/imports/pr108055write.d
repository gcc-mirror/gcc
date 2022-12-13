module imports.pr108055write;
import imports.pr108055spec;

void formatValueImpl(Writer, T, Char)(ref Writer , const(T) ,
                                      scope const ref FormatSpec!Char )
{
    T val;
    char spec;

    (ref val) @trusted {
        return (cast(const char*) &val)[0 .. val.sizeof];
    }(val);

}

void formatValue(Writer, T, Char)(Writer w, T val, Char f)
{
    formatValueImpl(w, val, f);
}
