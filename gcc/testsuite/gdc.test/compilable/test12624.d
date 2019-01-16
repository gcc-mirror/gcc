// REQUIRED_ARGS: -lib -Icompilable/extra-files
// EXTRA_FILES: extra-files/imp12624.d
// https://issues.dlang.org/show_bug.cgi?id=12624

struct SysTime
{
    import imp12624;

    Rebindable!(immutable TimeZone) _timezone = UTC();
}


class TimeZone
{
    this(string , string , string ) immutable {}
}


class UTC : TimeZone
{
    static immutable(UTC) opCall()
    {
        return _utc;
    }

    this() immutable {
        super("UTC", "UTC", "UTC");
    }

    static _utc = new immutable(UTC);
}
