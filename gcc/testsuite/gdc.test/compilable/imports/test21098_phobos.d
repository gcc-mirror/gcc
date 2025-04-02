struct Nullable(T)
{
    static struct DontCallDestructorT
    {
        T payload;
    }

    DontCallDestructorT _value;

    string toString() const
    {
        Appender!string app;
        formatValueImpl(app, _value);
        return null;
    }
}



struct Appender(A)
{
    InPlaceAppender!A impl;
}

struct InPlaceAppender(T)
{
    static void toStringImpl(const T[] data)
    {
        string app;
        formatValue(app, data);
    }
}



void formatValueImpl(Writer, T)(Writer, const(T)) {}

void formatValueImpl(Writer, T)(Writer w, T obj)
if (is(T == U[], U))
{
    formatValue(w, obj[0]);
}

enum HasToStringResult
{
    none,
    bla
}

template hasToString(T)
{
    static if (is(typeof(
        (T val) {
            val.toString(s);
        })))
        enum hasToString = HasToStringResult.bla;
    else
        enum hasToString = HasToStringResult.none;
}

void formatValueImpl(Writer, T)(ref Writer w, T val)
if (is(T == struct) || is(T == union))
{
    static if (hasToString!T)
        int dummy;
    formatElement(w, val.tupleof);
}

void formatElement(Writer, T)(Writer w, T val)
{
    formatValueImpl(w, val);
}

void formatValue(Writer, T)(Writer w, T val)
{
    formatValueImpl(w, val);
}
